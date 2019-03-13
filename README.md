
<!-- rmarkdown::render("README.Rmd") -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# mediascores: News-sharing Ideology from Social Media Link Data

![](https://travis-ci.org/desmarais-lab/NetworkInference.svg)

## About

This package provides an R implementation of a statistical model for
news media link data (URLs) on social media (e.g. Twitter, Facebook).
The goal of the method is to estimate:

1.  the news-sharing ideology of politicians and ordinary users on
    social media
2.  the ideology of the *content* that they share online

## Installation

The development version of the
[mediascores](https://github.com/SMAPPNYU/mediascores) library can be
installed in [R](https://r-project.org) with the following:

``` r
# install.packages("devtools")
devtools::install_github("SMAPPNYU/mediascores")
```

Note: a version of the
[mediascores](https://github.com/SMAPPNYU/mediascores) library will also
be made available later on [CRAN](https://cran.r-project.org/).

## Quick start guide

To get started, your data need to be formatted as a user-domain count
matrix, whereby rows represent individual social media users and columns
represent the news media (sub)domains that users share
(e.g. nytimes.com). Each cell in the matrix therefore represents the
number of times that a given user (row) shared a given domain (columns).

An example of such data (news domains tweeted by Members of Congress)
are included in the library and can be loaded as follows:

``` r
library(mediascores)
data(MOC115)
```

The default model can be fit to the data as follows:

``` r
# Note: This model will take a few hours to fit
fitted_model <- mediascores(Y = MOC115[, 7:151], group = MOC115$group,
                            anchors = c(1, 2),
                            chains = 4, cores = 4,
                            warmup = 750, iter = 1500)
```

## Vignette

A vignette describing the library in greater detail—the model,
model-fitting, convergence, and extracting quantities of interest—is
available
**[here](http://htmlpreview.github.com/?https://github.com/SMAPPNYU/mediascores/blob/master/vignettes/mediascores-vignette.html)**.
