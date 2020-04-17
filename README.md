
# genevoisgame

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/gopalpenny/genevoisgame.svg?branch=master)](https://travis-ci.org/gopalpenny/genevoisgame)
<!-- badges: end -->

The goal of genevoisgame is to ...

## Installation

You can install genevoisgame from [github](https://github.com/gopalpenny/genevoisgame) with:

``` r
devtools::install_github("https://github.com/gopalpenny/genevoisgame")
```

## Example

This simple example shows the functionality to evaluate a single game in a confined aquifer, given by `example_params_confined`:

``` r
library(genevoisgame)
evaluate_treaty(example_params_confined)
```

More than one game can be evaluated at a time, including for confined an unconfined aquifers and a range of socioeconomic and geophysical parameters. 

## Website

The package website can be found on [github.io](https://gopalpenny.github.io/genevoisgame)
