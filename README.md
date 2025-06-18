
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Spectrum regression testing

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This is a shiny app and package for comparing different runs of AIM via
extract.

## Installation

You can install the development version of
`{spectrum.regression.testing}` like so:

``` r
remotes::install_github("AvenirHealth-org/spectrum.regression.testing")
```

## Run

You can launch the application by running:

``` r
spectrum.regression.testing::run_app()
```

To run in development run the script `./scripts/run_dev.R`

## Data

This app uses data from 2 different sources

1.  Dropbox
2.  spectrum-orderly

To use dropbox data, we need to provide a path to the locally synced or
downloaded extract files. Run the `./scripts/initialize_data.R` to help
setup this environment. It will prompt you for a path to the “extracts”
directory which contains output from running the Spectrum extract tool.
Files will then be read from this directory.

TODO: Fill in details of working with the different data
