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
#' @param project_dir Character. Directory of the *project* you're adding
#'   this dependency to (where its `laplace.toml` and `laplace.lock` live or
#'   will be created). Defaults to the current working directory.
#'
#' @return Invisibly, the character vector of combined stdout/stderr lines
#'   from `laplace add`.
#'
#' @details
#' This function requires the `laplace` CLI to be installed and available on
#' `PATH` (see [laplace_install()]). Like `laplace add`, it creates the
#' project's `laplace.toml` and `laplace.lock` if they don't exist yet, so it
#' works in a fresh directory.
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
  if (!dir.exists(project_dir)) {
    stop("project_dir does not exist: ", project_dir, call. = FALSE)
  }

  ensure_laplace_cli()

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