#' Sample a model with a Laplace custom sampler
#'
#' Runs a model through `laplace-sample`, the optional custom-sampler
#' runtime of Laplace (BridgeStan backend), instead of CmdStan's NUTS. The
#' result is an ordinary cmdstanr fit, read from the CmdStan-style CSV files
#' `laplace-sample` writes, so `$summary()`, `$draws()` and
#' `$diagnostic_summary()` work as usual.
#'
#' @param model A `CmdStanModel` (its `.stan` file is used), or a path to a
#'   `.laplace` file (built with `laplace build` first) or a `.stan` file.
#' @param data A named list (written with [cmdstanr::write_stan_json()]), a
#'   path to a JSON data file, or `NULL` for a model without data.
#' @param sampler Built-in sampler: `"nuts"` (default), `"rwm"`, `"mala"` or
#'   `"hmc"`.
#' @param chains Number of chains. Default 4.
#' @param seed Integer seed, or `NULL` for a random one (recorded in the CSV
#'   header).
#' @param sampler_args Named list of sampler settings, passed as
#'   `--set name=value`, e.g. `list(step_size = 0.1, adapt = FALSE)`. See
#'   `laplace-sample sample --help` for each sampler's settings.
#' @param output_dir Directory for the CSV files. `NULL` (default) uses a new
#'   temporary directory.
#' @param iter_warmup,iter_sampling Warmup and saved iterations per chain.
#'   Default 1000 each.
#'
#' @return A `CmdStanMCMC` object from [cmdstanr::as_cmdstan_fit()].
#'
#' @details
#' Requires the `laplace-sample` binary on the `PATH` (and, for a
#' `.laplace` model, the `laplace` compiler). From a laplace checkout:
#' `cargo install --path laplace-sample --root ~/.local --force`.
#' Custom samplers are experimental in laplace 0.3.0.
#'
#' `laplace-sample` must be 0.3.0 or newer, and the same version as the
#' `laplace` compiler: the two are released together. `laplace_sample()`
#' asks both for `--version` and, on a mismatch (or no answer), warns with
#' both versions and the install command, then runs anyway.
#'
#' A NUTS diagnostic that does not apply to the sampler that ran is written
#' as `NaN` in every draw (`treedepth__`, `n_leapfrog__`, `divergent__`,
#' `energy__`): MALA and RWM have none of them, static HMC has no tree
#' depth. cmdstanr would still report `num_max_treedepth = 0` from an
#' all-`NaN` column (it drops the `NaN`s when it counts), so
#' `laplace_sample()` reads the columns after the fit and, when any is
#' all-`NaN`, says which diagnostics don't apply in one message. It runs
#' cmdstanr's usual divergence / tree-depth / E-BFMI checks only for the
#' columns that hold real values. `n_grad__` (gradient evaluations per draw),
#' `n_logp__` (density-only evaluations per draw, e.g. by `rwm`) and
#' `accept_stat__` are always present.
#'
#' @examples
#' \dontrun{
#' fit <- laplace_sample("model.laplace", data = list(N = 10, y = rnorm(10)),
#'                       sampler = "mala", seed = 1)
#' fit$summary()
#' }
#'
#' @export
laplace_sample <- function(model,
                           data = NULL,
                           sampler = "nuts",
                           chains = 4,
                           seed = NULL,
                           sampler_args = list(),
                           output_dir = NULL,
                           iter_warmup = 1000,
                           iter_sampling = 1000) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("laplace_sample() needs the `cmdstanr` package.", call. = FALSE)
  }
  bin <- Sys.which("laplace-sample")
  if (!nzchar(bin)) {
    stop(
      "Could not find the `laplace-sample` binary on your PATH.\n",
      "It is optional and installed separately; from a laplace checkout run\n",
      "  cargo install --path laplace-sample --root ~/.local --force",
      call. = FALSE
    )
  }

  problem <- sampler_version_problem(binary_version(bin), binary_version(Sys.which("laplace")))
  if (!is.null(problem)) {
    warning(problem, call. = FALSE)
  }

  model_path <- sample_model_path(model)
  if (is.null(output_dir)) {
    output_dir <- tempfile("laplace-sample-")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  data_path <- NULL
  if (is.list(data)) {
    data_path <- file.path(output_dir, "data.json")
    cmdstanr::write_stan_json(data, data_path)
  } else if (is.character(data)) {
    data_path <- normalizePath(data, mustWork = TRUE)
  } else if (!is.null(data)) {
    stop("`data` must be a named list, a JSON file path, or NULL.", call. = FALSE)
  }

  args <- laplace_sample_args(
    model_path, data_path, sampler, chains, seed, sampler_args,
    output_dir, iter_warmup, iter_sampling
  )
  # stdout: one CSV path per line. stderr (compile and progress messages)
  # goes straight to the console.
  out <- system2(bin, shQuote(args), stdout = TRUE, stderr = "")
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("`laplace-sample` failed (exit status ", status, "); see its output above.",
         call. = FALSE)
  }
  files <- normalizePath(out[nzchar(out)], mustWork = TRUE)

  # cmdstanr's own check runs every NUTS diagnostic; run only the ones whose
  # columns hold values, and say which do not apply.
  fit <- cmdstanr::as_cmdstan_fit(files, check_diagnostics = FALSE)
  nan_columns <- all_nan_columns(fit$sampler_diagnostics(format = "draws_df"))
  applicable <- applicable_diagnostics(nan_columns)
  if (length(applicable)) {
    fit$diagnostic_summary(diagnostics = applicable, quiet = FALSE)
  }
  note <- sample_diagnostics_note(sampler, nan_columns)
  if (!is.null(note)) {
    message(note)
  }
  fit
}

# The .stan or .laplace file behind `model`.
sample_model_path <- function(model) {
  if (inherits(model, "CmdStanModel")) {
    path <- model$stan_file()
  } else if (is.character(model) && length(model) == 1) {
    path <- model
  } else {
    stop("`model` must be a CmdStanModel or a path to a .laplace or .stan file.",
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Model file not found: ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

# Command-line arguments for `laplace-sample`.
laplace_sample_args <- function(model_path, data_path, sampler, chains, seed,
                                sampler_args, output_dir, iter_warmup,
                                iter_sampling) {
  if (length(sampler_args) && (is.null(names(sampler_args)) ||
                               any(!nzchar(names(sampler_args))))) {
    stop("`sampler_args` must be a named list, e.g. list(step_size = 0.1).",
         call. = FALSE)
  }
  args <- c("sample", model_path, "--sampler", sampler,
            "--chains", format(chains, scientific = FALSE),
            "--warmup", format(iter_warmup, scientific = FALSE),
            "--draws", format(iter_sampling, scientific = FALSE),
            "--output-dir", output_dir)
  if (!is.null(data_path)) {
    args <- c(args, "--data", data_path)
  }
  if (!is.null(seed)) {
    args <- c(args, "--seed", format(seed, scientific = FALSE))
  }
  for (name in names(sampler_args)) {
    value <- sampler_args[[name]]
    if (is.logical(value)) {
      value <- tolower(as.character(value))
    }
    args <- c(args, "--set", paste0(name, "=", format(value, scientific = FALSE)))
  }
  args
}

# The NUTS columns laplace-sample writes as NaN when they do not apply, in
# the order the note names them, with what each one diagnoses.
nuts_columns <- c(
  treedepth__ = "tree depth",
  n_leapfrog__ = "leapfrog steps",
  divergent__ = "divergences",
  energy__ = "E-BFMI"
)

# The NUTS columns that are NaN in every draw of every chain.
all_nan_columns <- function(diagnostics) {
  present <- intersect(names(nuts_columns), names(diagnostics))
  present[vapply(present, function(col) {
    v <- diagnostics[[col]]
    length(v) > 0 && all(is.nan(v))
  }, logical(1))]
}

# cmdstanr diagnostic_summary() checks whose columns hold real values.
applicable_diagnostics <- function(nan_columns) {
  checks <- c(divergences = "divergent__", treedepth = "treedepth__", ebfmi = "energy__")
  names(checks)[!checks %in% nan_columns]
}

# One message naming the diagnostics that don't apply, built from the
# all-NaN columns; NULL when every NUTS column has values. Tree depth is
# always named when its column is NaN, because cmdstanr would otherwise
# report num_max_treedepth = 0 for it.
sample_diagnostics_note <- function(sampler, nan_columns) {
  nan_columns <- names(nuts_columns)[names(nuts_columns) %in% nan_columns]
  if (!length(nan_columns)) {
    return(NULL)
  }
  what <- unname(nuts_columns[nan_columns])
  cols <- paste0("`", nan_columns, "`")
  list_words <- function(x) {
    if (length(x) == 1) x else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  }
  what_text <- list_words(what)
  substr(what_text, 1, 1) <- toupper(substr(what_text, 1, 1))
  sprintf(
    "%s %s apply to %s: %s %s NaN in every draw. See n_grad__ and accept_stat__ instead.",
    what_text, if (length(what) == 1 && what %in% c("tree depth", "E-BFMI")) "doesn't" else "don't",
    sampler, list_words(cols), if (length(cols) == 1) "is" else "are"
  )
}

# The oldest laplace-sample laplace_sample() works with: the first release,
# and the first to write the n_logp__ column documented above.
min_sampler_version <- "0.3.0"

# The version a laplace binary reports with `--version` ("laplace-sample
# 0.3.0", "laplace 0.3.0 (abc1234)"), or NULL when there is no binary or no
# usable answer.
binary_version <- function(bin) {
  if (!nzchar(bin)) {
    return(NULL)
  }
  out <- tryCatch(
    suppressWarnings(system2(bin, "--version", stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  status <- attr(out, "status")
  if (length(out) == 0 || (!is.null(status) && status != 0)) {
    return(NULL)
  }
  version <- strsplit(trimws(out[[1]]), "\\s+")[[1]][2]
  if (is.na(version) || is.na(package_version(version, strict = FALSE))) {
    return(NULL)
  }
  version
}

# laplace and laplace-sample are released together with one version. NULL
# when `sampler` is fine; otherwise the warning, naming both versions and the
# install command. `laplace` may be NULL (the compiler is not on PATH): then
# only the minimum is checked.
sampler_version_problem <- function(sampler, laplace) {
  want <- if (is.null(laplace)) min_sampler_version else laplace
  install <- sprintf(
    "install it with: cargo install --locked --git https://github.com/mlatinov/laplace --tag v%s laplace-sample",
    want
  )
  installed <- if (is.null(laplace)) "laplace is not on PATH" else sprintf("laplace is %s", laplace)
  if (is.null(sampler)) {
    return(sprintf(
      "Could not tell which laplace-sample this is (`laplace-sample --version` gave no version); %s, and laplace_sample() needs laplace-sample >= %s.\n%s",
      installed, min_sampler_version, install
    ))
  }
  too_old <- package_version(sampler) < package_version(min_sampler_version)
  differs <- !is.null(laplace) && sampler != laplace
  if (!too_old && !differs) {
    return(NULL)
  }
  why <- if (too_old) sprintf("laplace_sample() needs laplace-sample >= %s", min_sampler_version) else
    "the two are released together and should be the same version"
  sprintf("laplace-sample is %s but %s; %s.\n%s", sampler, installed, why, install)
}
