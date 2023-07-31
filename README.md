
<!-- README.md is generated from README.Rmd. Please edit that file -->

# susoquestionnairemanual shiny application

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This application is part of the large set of tools, to facilitate survey
implementation with [Survey
Solutions](https://docs.mysurvey.solutions/). Questionnaires created in
[Survey Solutions designer](https://designer.mysurvey.solutions/), can
either be loaded directly from the data collection server, or through
the upload of an exported json string.

## Installation

- Install R: <https://cran.r-project.org/mirrors.html> (version 4.1.1 or
  greater)

- Install R Studio: <https://rstudio.com/products/rstudio/download/>
  (version 1.2.5001-3 or newer)

- Make sure the *devtools* package is installed, if not install it with:

``` r
install.packages("devtools")
```

- After that install the actual package:

``` r
devtools::install_github("michael-cw/susoquestionnairemanual")
```

## Start the application

``` r
library(susoquestionnairemanual)
susoquestionnairemanual::runQuestManualApp()
```

## Start the application on a shiny server

In case you are planning to run the application on a shiny server, you
just need to create the following app.R script in your shiny server app
directory:

``` r
library(susoquestionnairemanual)
susoquestionnairemanual::runQuestManualAppServer()
```
