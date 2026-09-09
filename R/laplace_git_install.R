#' Add a git-hosted Laplace library as a dependency
#'
#' Thin wrapper around `laplace add --git`, for adding a `.laplacelib`
#' library dependency straight from a git repository without hand-assembling
#' the CLI flags each time. Requires exactly one of `tag` or `rev` to pin the
#' dependency, matching what `laplace add --git` itself requires.
#'
#' @param name Character. The package name to register the dependency under
#'   (as it will be used in `pkg::func()` calls and in `laplace.toml`).
#' @param repo Character. Git URL of the repository, e.g.
#'   `"https://github.com/user/some-laplace-lib"`.
#' @param tag Character or `NULL`. Git tag to pin to (e.g. `"0.1.0"`). Give
#'   exactly one of `tag` or `rev`.
#' @param rev Character or `NULL`. Git commit SHA to pin to instead of a tag.
#'   Give exactly one of `tag` or `rev`.
#' @param subdir Character or `NULL`. Subdirectory within the repository
#'   where the library's `laplace.toml` lives, if it isn't at the repo root.
#' @param project_dir Character. Directory containing the `laplace.toml` of
#'   the *project* you're adding this dependency to (not the dependency's
#'   own manifest). Defaults to the current working directory.
#'
#' @return Invisibly, the character vector of combined stdout/stderr lines
#'   from `laplace add`.
#'
#' @details
#' This function requires the `laplace` CLI to be installed and available on
#' `PATH` (see [laplace_install()]) and `project_dir` to contain a
#' `laplace.toml` for the project you're adding the dependency to — this
#' function does not create one.
#'
#' @examples
#' \dontrun{
#' laplace_install_git(
#'   "transformations",
#'   "https://github.com/mlatinov/laplace-transform",
#'   tag = "0.1.0",
#'   subdir = "laplace"
#' )
#'
#' # Pin to a commit instead of a tag
#' laplace_install_git(
#'   "transformations",
#'   "https://github.com/mlatinov/laplace-transform",
#'   rev = "a35b3f2"
#' )
#' }
#'
#' @export
laplace_install_git <- function(name,
                                 repo,
                                 tag = NULL,
                                 rev = NULL,
                                 subdir = NULL,
                                 project_dir = ".") {

  if (is.null(tag) && is.null(rev)) {
    stop("Provide exactly one of `tag` or `rev` to pin the dependency.", call. = FALSE)
  }
  if (!is.null(tag) && !is.null(rev)) {
    stop("Provide only one of `tag` or `rev`, not both.", call. = FALSE)
  }

  project_dir <- path.expand(project_dir)
  manifest_path <- file.path(project_dir, "laplace.toml")
  if (!file.exists(manifest_path)) {
    stop(
      "No laplace.toml found in project_dir: ", project_dir, "\n",
      "laplace_install_git() adds a dependency to an existing laplace project;\n",
      "run laplace_model()/`laplace init` there first, or pass the correct project_dir.",
      call. = FALSE
    )
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
        "Call laplace_install() to install it automatically, or see\n",
        "https://github.com/mlatinov/laplace for manual installation instructions.",
        call. = FALSE
      )
    }
  }

  args <- c("add", name, "--git", repo)
  if (!is.null(tag)) args <- c(args, "--tag", tag)
  if (!is.null(rev)) args <- c(args, "--rev", rev)
  if (!is.null(subdir)) args <- c(args, "--subdir", subdir)

  old_wd <- getwd()
  setwd(project_dir)
  on.exit(setwd(old_wd), add = TRUE)

  add_result <- system2(
    "laplace",
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(add_result, "status")
  if (!is.null(status) && status != 0) {
    stop(
      "`laplace add` failed for `", name, "`:\n",
      paste(add_result, collapse = "\n"),
      call. = FALSE
    )
  }

  message(paste(add_result, collapse = "\n"))
  invisible(add_result)
}