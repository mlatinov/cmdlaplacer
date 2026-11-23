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

test_that("the diagnostics note names non-NUTS samplers only", {
  expect_null(sample_diagnostics_note("nuts"))
  expect_identical(
    sample_diagnostics_note("mala"),
    "Tree depth and divergence diagnostics don't apply to mala; see n_grad__ and accept_stat__ instead."
  )
  for (s in c("rwm", "hmc")) {
    expect_match(sample_diagnostics_note(s), paste0("don't apply to ", s, ";"), fixed = TRUE)
  }
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
    "Tree depth and divergence diagnostics don't apply to mala; see n_grad__ and accept_stat__ instead.",
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

test_that("NUTS runs without the note, and data and sampler_args pass through", {
  skip_without_sampler()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "normal.stan")
  writeLines(c("data { real mu; }", "parameters { real x; }", "model { x ~ normal(mu, 1); }"), path)
  expect_no_message(
    fit <- laplace_sample(path, data = list(mu = 3), chains = 2, seed = 2,
                          sampler_args = list(max_depth = 5),
                          iter_warmup = 200, iter_sampling = 200),
    message = "don't apply"
  )
  expect_equal(fit$summary("x")$mean, 3, tolerance = 0.3)
  expect_match(fit$metadata()$laplace_settings, "max_depth:5", fixed = TRUE)
  expect_false(any(is.nan(fit$diagnostic_summary(quiet = TRUE)$num_divergent)))
})
