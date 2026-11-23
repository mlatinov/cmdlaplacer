# Unit tests run everywhere; the end-to-end tests need the laplace-sample
# binary (and a C++ toolchain for its first model build) and are skipped
# without it.

std_normal_stan <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(dir, "std_normal.stan")
  writeLines(c("parameters {", "  vector[2] x;", "}", "model {", "  x ~ std_normal();", "}"), path)
  path
}

skip_without_sampler <- function() {
  skip_if_not_installed("cmdstanr")
  skip_if(!nzchar(Sys.which("laplace-sample")), "laplace-sample is not on PATH")
}

draws_with <- function(...) {
  cols <- list(...)
  n <- 4
  base <- list(accept_stat__ = runif(n), stepsize__ = rep(0.5, n), treedepth__ = rep(2, n),
               n_leapfrog__ = rep(3, n), divergent__ = rep(0, n), energy__ = rnorm(n),
               n_grad__ = rep(3, n))
  base[names(cols)] <- cols
  posterior::as_draws_df(base)
}

test_that("the note is built from the all-NaN columns", {
  skip_if_not_installed("posterior")
  # NUTS: every column has values -> no note, every check runs.
  nuts <- all_nan_columns(draws_with())
  expect_identical(nuts, character())
  expect_null(sample_diagnostics_note("nuts", nuts))
  expect_identical(applicable_diagnostics(nuts), c("divergences", "treedepth", "ebfmi"))

  # MALA / RWM: all four NaN.
  mala <- all_nan_columns(draws_with(treedepth__ = rep(NaN, 4), n_leapfrog__ = rep(NaN, 4),
                                     divergent__ = rep(NaN, 4), energy__ = rep(NaN, 4)))
  expect_identical(mala, c("treedepth__", "n_leapfrog__", "divergent__", "energy__"))
  expect_identical(
    sample_diagnostics_note("mala", mala),
    paste("Tree depth, leapfrog steps, divergences and E-BFMI don't apply to mala:",
          "`treedepth__`, `n_leapfrog__`, `divergent__` and `energy__` are NaN in every draw.",
          "See n_grad__ and accept_stat__ instead.")
  )
  expect_identical(applicable_diagnostics(mala), character())

  # Static HMC: only tree depth is NaN; divergences and E-BFMI still checked.
  hmc <- all_nan_columns(draws_with(treedepth__ = rep(NaN, 4)))
  expect_identical(hmc, "treedepth__")
  expect_identical(
    sample_diagnostics_note("hmc", hmc),
    "Tree depth doesn't apply to hmc: `treedepth__` is NaN in every draw. See n_grad__ and accept_stat__ instead."
  )
  expect_identical(applicable_diagnostics(hmc), c("divergences", "ebfmi"))

  expect_match(sample_diagnostics_note("x", "divergent__"), "^Divergences don't apply to x:")
  expect_match(sample_diagnostics_note("x", "energy__"), "^E-BFMI doesn't apply to x:")

  # Partly NaN is not "doesn't apply".
  expect_identical(all_nan_columns(draws_with(energy__ = c(NaN, 1, 2, 3))), character())
})

test_that("arguments map onto laplace-sample flags", {
  args <- laplace_sample_args("/m/model.stan", "/m/data.json", "hmc", 2, 42,
                              list(step_size = 0.1, adapt = FALSE, steps = 20),
                              "/out", 500, 1e5)
  expect_identical(args, c(
    "sample", "/m/model.stan", "--sampler", "hmc", "--chains", "2",
    "--warmup", "500", "--draws", "100000", "--output-dir", "/out",
    "--data", "/m/data.json", "--seed", "42",
    "--set", "step_size=0.1", "--set", "adapt=false", "--set", "steps=20"
  ))
  no_data <- laplace_sample_args("/m.stan", NULL, "nuts", 4, NULL, list(), "/o", 1000, 1000)
  expect_false(any(c("--data", "--seed", "--set") %in% no_data))
  expect_error(laplace_sample_args("/m.stan", NULL, "nuts", 4, NULL, list(0.1), "/o", 1, 1),
               "named list")
})

test_that("model may be a path or a CmdStanModel", {
  path <- std_normal_stan()
  expect_identical(sample_model_path(path), normalizePath(path))
  expect_error(sample_model_path(file.path(tempdir(), "nope.stan")), "not found")
  expect_error(sample_model_path(42), "CmdStanModel or a path")
  skip_if_not_installed("cmdstanr")
  mod <- cmdstanr::cmdstan_model(path, compile = FALSE)
  expect_identical(sample_model_path(mod), normalizePath(path))
})

test_that("a missing laplace-sample binary gives an install hint", {
  skip_if_not_installed("cmdstanr")
  withr::local_envvar(PATH = withr::local_tempdir())
  expect_error(laplace_sample(std_normal_stan()), "cargo install --path laplace-sample")
})

test_that("laplace_sample() returns a cmdstanr fit and notes non-NUTS diagnostics", {
  skip_without_sampler()
  path <- std_normal_stan()
  expect_no_warning(expect_message(
    fit <- laplace_sample(path, sampler = "mala", chains = 2, seed = 1,
                          iter_warmup = 200, iter_sampling = 200),
    paste("Tree depth, leapfrog steps, divergences and E-BFMI don't apply to mala:",
          "`treedepth__`, `n_leapfrog__`, `divergent__` and `energy__` are NaN in every draw.",
          "See n_grad__ and accept_stat__ instead."),
    fixed = TRUE
  ))
  expect_s3_class(fit, "CmdStanMCMC")
  s <- fit$summary()
  expect_setequal(s$variable, c("lp__", "x[1]", "x[2]"))
  expect_equal(dim(fit$draws()), c(200, 2, 3))
  ds <- suppressWarnings(fit$diagnostic_summary(quiet = TRUE))
  expect_true(all(is.nan(ds$num_divergent)))
  expect_match(fit$metadata()$laplace_kernel_rest, "^mala [0-9.]+ sha256:[0-9a-f]{64}$")
  expect_match(fit$metadata()$laplace_sampler, "^mala \\(sha256: [0-9a-f]{64}\\)$")
})

test_that("for hmc the note names only tree depth", {
  skip_without_sampler()
  expect_message(
    fit <- laplace_sample(std_normal_stan(), sampler = "hmc", chains = 2, seed = 3,
                          iter_warmup = 200, iter_sampling = 200),
    "Tree depth doesn't apply to hmc: `treedepth__` is NaN in every draw. See n_grad__ and accept_stat__ instead.",
    fixed = TRUE
  )
  ds <- fit$diagnostic_summary(quiet = TRUE)
  expect_false(any(is.nan(ds$num_divergent)))
})

test_that("NUTS runs without the note, and data and sampler_args pass through", {
  skip_without_sampler()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "normal.stan")
  writeLines(c("data { real mu; }", "parameters { real x; }", "model { x ~ normal(mu, 1); }"), path)
  expect_no_message(
    fit <- laplace_sample(path, data = list(mu = 3), chains = 2, seed = 2,
                          sampler_args = list(max_depth = 5),
                          iter_warmup = 200, iter_sampling = 200),
    message = "apply to"
  )
  expect_equal(fit$summary("x")$mean, 3, tolerance = 0.3)
  expect_match(fit$metadata()$laplace_settings, "max_depth:5", fixed = TRUE)
  expect_false(any(is.nan(fit$diagnostic_summary(quiet = TRUE)$num_divergent)))
})

# A fake binary that answers `--version` with `line` (or nothing, and fails).
fake_binary <- function(name, line, dir) {
  path <- file.path(dir, name)
  body <- if (is.null(line)) "exit 2" else sprintf("echo '%s'", line)
  writeLines(c("#!/bin/sh", body), path)
  Sys.chmod(path, "755")
  path
}

test_that("versions are read from --version", {
  skip_on_os("windows")
  dir <- withr::local_tempdir()
  expect_identical(binary_version(fake_binary("ls", "laplace-sample 0.3.0", dir)), "0.3.0")
  expect_identical(binary_version(fake_binary("lc", "laplace 0.3.0 (abc1234)", dir)), "0.3.0")
  expect_null(binary_version(fake_binary("none", NULL, dir)), "no output")
  expect_null(binary_version(fake_binary("junk", "hello", dir)), "no version in the output")
  expect_null(binary_version(""), "not on PATH")
})

test_that("matching versions are quiet", {
  expect_null(sampler_version_problem("0.3.0", "0.3.0"))
  expect_null(sampler_version_problem("0.4.1", NULL), "no laplace: only the minimum")
})

test_that("a mismatch or a too-old laplace-sample warns with both versions and the install command", {
  differs <- sampler_version_problem("0.3.0", "0.4.0")
  expect_match(differs, "laplace-sample is 0.3.0 but laplace is 0.4.0", fixed = TRUE)
  expect_match(differs, "released together", fixed = TRUE)
  expect_match(differs, "cargo install --locked --git https://github.com/mlatinov/laplace --tag v0.4.0 laplace-sample",
               fixed = TRUE)

  old <- sampler_version_problem("0.1.0", NULL)
  expect_match(old, "laplace-sample is 0.1.0 but laplace is not on PATH", fixed = TRUE)
  expect_match(old, "needs laplace-sample >= 0.3.0", fixed = TRUE)
  expect_match(old, "--tag v0.3.0 laplace-sample", fixed = TRUE)
})

test_that("no version output warns too", {
  msg <- sampler_version_problem(NULL, "0.3.0")
  expect_match(msg, "Could not tell which laplace-sample this is", fixed = TRUE)
  expect_match(msg, "laplace is 0.3.0", fixed = TRUE)
  expect_match(msg, "--tag v0.3.0 laplace-sample", fixed = TRUE)
})

test_that("laplace_sample() warns about a mismatched laplace-sample and still runs", {
  skip_on_os("windows")
  skip_if_not_installed("cmdstanr")
  dir <- withr::local_tempdir()
  # A laplace-sample that reports 0.1.0, then fails the run itself: the
  # warning comes first, and the call goes on to run it.
  writeLines(c("#!/bin/sh", "if [ \"$1\" = --version ]; then echo 'laplace-sample 0.1.0'; exit 0; fi",
               "echo ran >&2", "exit 3"), file.path(dir, "laplace-sample"))
  Sys.chmod(file.path(dir, "laplace-sample"), "755")
  withr::local_path(dir, action = "prefix")
  # Every warning is collected: system2() adds its own for the failed run.
  warnings <- character()
  withCallingHandlers(
    expect_error(laplace_sample(std_normal_stan()), "`laplace-sample` failed (exit status 3)", fixed = TRUE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("laplace-sample is 0.1.0", warnings, fixed = TRUE)), info = paste(warnings, collapse = "\n"))
})
