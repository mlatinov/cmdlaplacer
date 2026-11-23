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
#'
#' For a sampler other than NUTS, tree depth and divergences are not
#' defined: those CSV columns hold `NaN`, `$diagnostic_summary()` shows `NaN`
#' divergences and an `NA` E-BFMI, and still reports `num_max_treedepth = 0`
#' (cmdstanr drops the `NaN`s when it counts). `laplace_sample()` says so in
#' a message after the fit; `n_grad__` (gradient evaluations per draw) and
#' `accept_stat__` are the diagnostics to look at instead.
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

  note <- sample_diagnostics_note(sampler)
  # cmdstanr's diagnostic check is NUTS-specific: for any other sampler it
  # only warns that E-BFMI cannot be computed from the NaN energy__ column.
  # The note below says what applies instead.
  fit <- cmdstanr::as_cmdstan_fit(files, check_diagnostics = is.null(note))
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

# One message for samplers where tree depth and divergences don't apply
# (their CSV columns are NaN, and cmdstanr still reports num_max_treedepth = 0).
sample_diagnostics_note <- function(sampler) {
  if (identical(sampler, "nuts")) {
    return(NULL)
  }
  sprintf(
    "Tree depth and divergence diagnostics don't apply to %s; see n_grad__ and accept_stat__ instead.",
    sampler
  )
}
