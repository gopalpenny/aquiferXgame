
# genevoisgame

<!-- badges: start -->
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

## Vignette

For more details, see the full vignette on [github.io](https://gopalpenny.github.io/genevoisgame/genevoisgame-vignette.html)
