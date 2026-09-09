#' Compile a Laplace model and load it with cmdstanr
#'
#' Thin wrapper around `laplace build` that compiles a `.laplace` source file
#' to plain Stan and hands the resulting `.stan` file to
#' [cmdstanr::cmdstan_model()]. Laplace is never a runtime dependency of the
#' compiled model — this function only shells out to the `laplace` CLI at
#' build time and always writes a real, inspectable `.stan` file to disk
#' before doing anything else with it.
#'
#' @param path Character. Path to the `.laplace` source file to build.
#' @param stan_only Logical. If `TRUE`, skip cmdstanr entirely and return the
#'   path to the compiled `.stan` file instead of a `cmdstanr` model object.
#'   Use this if you want to drive cmdstanr (or anything else) yourself.
#'   Default `FALSE`.
#' @param out_dir Character or `NULL`. Directory to write the compiled
#'   `.stan` file to. If `NULL` (the default), uses the same directory as
#'   `path` when `stan_only = TRUE`, and a temporary directory otherwise.
#' @param ... Additional arguments passed through to
#'   [cmdstanr::cmdstan_model()] (e.g. `compile = FALSE`, `cpp_options`).
#'   Ignored when `stan_only = TRUE`.
#'
#' @return If `stan_only = FALSE` (default), a `CmdStanModel` object as
#'   returned by [cmdstanr::cmdstan_model()]. If `stan_only = TRUE`, a
#'   character string giving the path to the compiled `.stan` file.
#'
#' @details
#' This function requires the `laplace` CLI to be installed and available on
#' the system `PATH`. If it isn't found, an informative error is raised
#' rather than a cryptic `system2` failure. See
#' <https://github.com/mlatinov/laplace_tools> for installation instructions.
#'
#' @examples
#' \dontrun{
#' # Build and load directly as a cmdstanr model
#' mod <- laplace_model("models/linreg.laplace")
#' fit <- mod$sample(data = list(N = 10, y = rnorm(10)))
#'
#' # Escape hatch: just get the compiled .stan file and do your own thing
#' stan_path <- laplace_model("models/linreg.laplace", stan_only = TRUE)
#' }
#'
#' @export
laplace_model <- function(path, stan_only = FALSE, out_dir = NULL, ...) {

  if (!file.exists(path)) {
    stop("Laplace source file not found: ", path, call. = FALSE)
  }

  if (nzchar(Sys.which("laplace")) == FALSE) {

    if (interactive()) {
      install_now <- isTRUE(utils::askYesNo(
        "The `laplace` CLI was not found on your PATH. Install it now via cargo? (requires git and cargo)"
      ))
      if (isTRUE(install_now)) {
        laplace_install()
      }
    }

    if (nzchar(Sys.which("laplace")) == FALSE) {
      stop(
        "Could not find the `laplace` CLI on your PATH.\n",
        "laplace_model() requires the Laplace compiler to be installed.\n",
        "Call laplace_install() to install it automatically, or see\n",
        "https://github.com/mlatinov/laplace for manual installation instructions.",
        call. = FALSE
      )
    }
  }

  if (is.null(out_dir)) {
    out_dir <- if (isTRUE(stan_only)) dirname(path) else tempdir()
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  build_result <- system2(
    "laplace",
    args = c("build", shQuote(path), "--out", shQuote(out_dir)),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(build_result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "`laplace build` failed for ", path, ":\n",
      paste(build_result, collapse = "\n"),
      call. = FALSE
    )
  }

  stan_path <- file.path(out_dir, sub("\\.laplace$", ".stan", basename(path)))

  if (!file.exists(stan_path)) {
    stop(
      "`laplace build` did not produce the expected output file: ", stan_path, "\n",
      "Build output:\n",
      paste(build_result, collapse = "\n"),
      call. = FALSE
    )
  }

  if (isTRUE(stan_only)) {
    return(stan_path)
  }

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop(
      "The `cmdstanr` package is required unless stan_only = TRUE.\n",
      "Install it from https://mc-stan.org/cmdstanr/, or call\n",
      "laplace_model(..., stan_only = TRUE) to get the .stan path instead.",
      call. = FALSE
    )
  }

  cmdstanr::cmdstan_model(stan_path, ...)
}