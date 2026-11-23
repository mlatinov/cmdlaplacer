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
