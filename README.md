
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

## Setup

This app uses data from 2 different sources

1.  Manual extracts from dropbox
2.  [spectrum-orderly](https://github.com/avenirHealth-org/spectrum-orderly)

This repo uses [dotenv](https://github.com/gaborcsardi/dotenv) to manage
configuration via a `.env` file. To run the ShinyApp we need to provide
a path to locally synced or downloaded extract files, and a path to the
spectrum-orderly root.

1.  Sync the extracts folder from Dropbox [Avenir Shared
    Drive/Projects/Models/AIM/Shiny regression
    testing](https://www.dropbox.com/scl/fo/7s2euzmaochus4gn1m6fx/AHCiBYBtAOlgbO644mNdFh4?rlkey=xae0wwt1h7qumxh3i8cx129c0&st=smwhkw95&dl=0)
    note the local path.

2.  Sync the orderly remote from Programming Sharepoint
    [Documents/spectrum-orderly](https://futuresinstitute.sharepoint.com/:f:/s/Programming/Es57cTFvF_tKv0KzTKacj_sBaCtvQKke_UtfB8_dzE-LzQ?e=leTSxM)
    note the local path

3.  run the `./scripts/initialize_data.R` to create the `.env` file for
    you or write the paths into a file at the root of this repo `.env`
    e.g.

        EXTRACTS_DIR=C:\Users\Test\Avenir Health Dropbox\Avenir Shared Drive\Projects\Models\AIM\Shiny regression testing\extracts
        ORDERLY_ROOT=C:\Users\Test\Avenir Health\Programming - spectrum-orderly

## Run

You can launch the application by running:

``` r
spectrum.regression.testing::run_app()
```

To run in development run the script `./scripts/run_dev.R` or source the
app in an R session with `devtools::load_all()` and then run the app
with `run_app()`

## Updating data

### New orderly run

The orderly model fit task is run automatically once a week. You can
manually run it by going to the [spectrum-orderly-runner run AIM GitHub
actions](https://github.com/AvenirHealth-org/spectrum-orderly-runner/actions/workflows/run-aim.yaml)

1.  Click the “Run workflow” at the top right below the list of workflow
    runs
2.  Always use the “main” branch here, but you can select the version of
    spectrum desktop to install. This must match one of the [release
    tags from Spec5
    repo](https://github.com/AvenirHealth-org/Spec5/releases) e.g. v6.42
    or v6.43-beta.3. If not specified it will use the release tagged as
    “latest”
3.  The CI will install the specified version of Spectrum and then run
    Extract via the CLI and save the results. The orderly task will also
    record the hash of the Spec5 repo for this Spectrum version in
    metadata. It records the Spectrum version in parameters. These can
    be used for querying downstream.

Note that running via the CI like this will only work for versions of
Spectrum for which a release is available. If you want to run it before
a release has been made then it is best to run the orderly task
manually. See the [spectrum-orderly
repo](https://github.com/AvenirHealth-org/spectrum-orderly/) for details
of how to do this.

### Updating the extract configuration

The extract configuration is stored in the [orderly repo shared
directory](https://github.com/AvenirHealth-org/spectrum-orderly/blob/main/shared/aim_extract_template.ex)
update this here the configuration is used in this ShinyApp to know what
indicators are available for display. The configuration from the orderly
run will be read and used automatically.

### Displaying new indicators

Update the `metadata/indicator-table.csv`
