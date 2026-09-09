#' Install the Laplace CLI
#'
#' Builds and installs the `laplace` command-line compiler from source,
#' using `git` to fetch it and `cargo` to build it — mirroring the manual
#' installation steps documented at
#' <https://github.com/mlatinov/laplace#installing-laplace>. This is a
#' convenience wrapper only: it does not vendor or patch anything, and the
#' resulting binary is exactly what you'd get by running the same `git`
#' and `cargo` commands yourself.
#'
#' `laplace_model()` will offer to call this function automatically in
#' interactive sessions when the `laplace` CLI can't be found. You can also
#' call it directly.
#'
#' @param root Character. Install root passed to `cargo install --root`.
#'   The binary ends up at `file.path(root, "bin", "laplace")`. Defaults to
#'   `"~/.local"`, matching the upstream documentation.
#' @param repo Character. Git URL to clone. Defaults to the main Laplace
#'   repository.
#' @param ref Character or `NULL`. Optional git branch or tag to check out
#'   (passed to `git clone --branch`). `NULL` (the default) uses the
#'   repository's default branch.
#' @param quiet Logical. If `TRUE`, suppress progress messages. Errors are
#'   always raised regardless of this setting. Default `FALSE`.
#'
#' @return Invisibly, the path to the installed `laplace` binary.
#'
#' @details
#' Requires `git` and a stable Rust toolchain (`cargo`) to already be
#' installed and on `PATH`; this function does not install either of those
#' for you. Building `laplace` itself does not require Stan or `stanc` —
#' those are only needed for the optional `--validate` flag on
#' `laplace build`.
#'
#' If the install directory's `bin/` is not already on `PATH`, it is added
#' to `PATH` for the current R session only (via `Sys.setenv()`), and a
#' message explains how to make that permanent in your shell profile.
#'
#' `cargo install` is always called with `--force`. Without it, Cargo
#' silently skips reinstalling when it thinks the same version is already
#' present, which would leave a stale binary in place if the source changed
#' without a version bump in `Cargo.toml` — exactly the kind of thing a
#' "reinstall" helper should not do quietly.
#'
#' @examples
#' \dontrun{
#' laplace_install()
#'
#' # Install a specific tag into a custom location
#' laplace_install(root = "~/.laplace", ref = "v0.3.0")
#' }
#'
#' @export
laplace_install <- function(root = "~/.local",
                             repo = "https://github.com/mlatinov/laplace",
                             ref = NULL,
                             quiet = FALSE) {

  if (nzchar(Sys.which("git")) == FALSE) {
    stop(
      "`git` is required to install laplace but was not found on your PATH.\n",
      "Install git and try again.",
      call. = FALSE
    )
  }

  if (nzchar(Sys.which("cargo")) == FALSE) {
    stop(
      "`cargo` (Rust) is required to install laplace but was not found on your PATH.\n",
      "Install a stable Rust toolchain from https://rustup.rs and try again.",
      call. = FALSE
    )
  }

  root <- path.expand(root)
  src_dir <- tempfile("laplace-src-")
  on.exit(unlink(src_dir, recursive = TRUE, force = TRUE), add = TRUE)

  clone_args <- c("clone", "--depth", "1")
  if (!is.null(ref)) {
    clone_args <- c(clone_args, "--branch", ref)
  }
  clone_args <- c(clone_args, repo, src_dir)

  if (!quiet) message("Cloning laplace from ", repo, " ...")
  clone_result <- system2("git", clone_args, stdout = TRUE, stderr = TRUE)
  status <- attr(clone_result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "git clone failed:\n",
      paste(clone_result, collapse = "\n"),
      call. = FALSE
    )
  }

  if (!dir.exists(root)) {
    dir.create(root, recursive = TRUE)
  }

  if (!quiet) message("Building laplace with cargo (this may take a minute) ...")
  install_result <- system2(
    "cargo",
    c("install", "--path", shQuote(src_dir), "--root", shQuote(root), "--force"),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(install_result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "cargo install failed:\n",
      paste(install_result, collapse = "\n"),
      call. = FALSE
    )
  }

  bin_dir <- file.path(root, "bin")
  bin_name <- if (.Platform$OS.type == "windows") "laplace.exe" else "laplace"
  bin_path <- file.path(bin_dir, bin_name)

  if (!file.exists(bin_path)) {
    stop(
      "cargo install completed but the laplace binary was not found at ",
      bin_path, ".",
      call. = FALSE
    )
  }

  path_dirs <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  on_path <- normalizePath(bin_dir, mustWork = FALSE) %in%
    normalizePath(path_dirs, mustWork = FALSE)

  if (!on_path) {
    Sys.setenv(PATH = paste(bin_dir, Sys.getenv("PATH"), sep = .Platform$path.sep))
    if (!quiet) {
      message(
        "laplace installed to ", bin_path, "\n",
        "Added ", bin_dir, " to PATH for this R session only.\n",
        "To make this permanent, add this line to your shell profile:\n",
        "  export PATH=\"", bin_dir, ":$PATH\""
      )
    }
  } else if (!quiet) {
    message("laplace installed to ", bin_path)
  }

  invisible(bin_path)
}