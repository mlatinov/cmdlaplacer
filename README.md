# cmdlaplacer

A thin R wrapper around the [Laplace](https://github.com/mlatinov/laplace_tools) compiler and [cmdstanr](https://mc-stan.org/cmdstanr/), so you can go from a `.laplace` source file to a running `cmdstanr` model without touching the terminal.

## Why

Laplace compiles `.laplace` files to plain, inspectable Stan (`.stan`) source — that output is always the source of truth and never depends on Laplace at runtime. This package doesn't change that. It just removes the manual step of running `laplace build` yourself and pointing `cmdstanr::cmdstan_model()` at the result.

- **Default path**: give it a `.laplace` file, get back a ready-to-use `CmdStanModel` object.
- **Escape hatch**: if you'd rather handle the Stan file yourself, ask for the path and do your own thing from there.

Nothing about compilation, caching, or diagnostics happens in R — that's all delegated to the `laplace` binary. This package is intentionally small.

## Installation

`cmdlaplacer` requires two things to be installed separately first:

1. **The `laplace` CLI**, available on your system `PATH`.
2. **cmdstanr**, plus a working CmdStan installation. See the [cmdstanr installation guide](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

Then install the package itself:

```r
# install.packages("remotes")
remotes::install_github("mlatinov/laplace_tools", subdir = "cmdlaplacer")
```

### Installing the `laplace` CLI

`laplace` is built from source with Cargo. If you already have `git` and a Rust toolchain (`cargo`) on your `PATH`, you can install it directly from R:

```r
library(cmdlaplacer)
laplace_install()
```

This clones the [Laplace repository](https://github.com/mlatinov/laplace), builds it with `cargo install --path . --root ~/.local`, and adds `~/.local/bin` to `PATH` for the current session. It's the same thing you'd get from running those `git`/`cargo` commands by hand — see the [manual installation instructions](https://github.com/mlatinov/laplace#installing-laplace) if you'd rather do that yourself, or need to install `git`/Rust first.

You don't need to call `laplace_install()` yourself, though: if `laplace_model()` can't find `laplace` on your `PATH` in an interactive session, it will ask whether to install it for you before failing.

## Usage

```r
library(cmdlaplacer)

# Compile a .laplace file and load it directly as a cmdstanr model
mod <- laplace_model("models/linreg.laplace")
fit <- mod$sample(data = list(N = 10, y = rnorm(10)))
```

If you'd rather work with the compiled Stan file directly — to inspect it, version it, or drive cmdstanr yourself — pass `stan_only = TRUE`:

```r
stan_path <- laplace_model("models/linreg.laplace", stan_only = TRUE)

# now do whatever you'd normally do with a .stan file
mod <- cmdstanr::cmdstan_model(stan_path)
```

### Arguments

| Argument | Description |
|---|---|
| `path` | Path to the `.laplace` source file to build. |
| `stan_only` | If `TRUE`, return the path to the compiled `.stan` file instead of a `cmdstanr` model object. Default `FALSE`. |
| `out_dir` | Where to write the compiled `.stan` file. Defaults to the source file's own directory when `stan_only = TRUE`, or a temp directory otherwise. |
| `...` | Passed straight through to `cmdstanr::cmdstan_model()` (e.g. `compile = FALSE`). |

## What this package does *not* do

- It does not reimplement or wrap `cmdstanr`'s fitting API (`$sample()`, `$optimize()`, etc.) — use `cmdstanr` directly on the returned model object.
- It does not cache builds in R — `laplace build` is responsible for deciding whether a rebuild is needed.
- It does not hide the compiled Stan source — the `.stan` file is always written to disk and can be inspected, diffed, or committed independently of this package.

## Error handling

If the `laplace` CLI isn't found on your `PATH`:

- In an **interactive** session, `laplace_model()` asks whether to install it via `laplace_install()` before failing.
- In a **non-interactive** session (scripts, `Rscript`, CI), it never prompts — it just raises an informative error pointing at `laplace_install()` or the manual installation instructions.

If `laplace build` fails, or if `cmdstanr` isn't installed and you haven't set `stan_only = TRUE`, you also get a clear, specific error rather than a raw system-call failure or package-loading error.

## License

MIT