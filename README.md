
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phylepic

<!-- badges: start -->

[![r-universe
status](https://cidm-ph.r-universe.dev/badges/phylepic)](https://cidm-ph.r-universe.dev)
[![R-CMD-check](https://github.com/cidm-ph/phylepic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cidm-ph/phylepic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Phylepic contains tools for visualisations that are useful for genomic
epidemiology of pathogens, designed for a public health setting.

## Installation

You can install phylepic like so:

``` r
# development version
install.packages('phylepic', repos = c('https://cidmp-ph.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

This is an example of a very minimal phylepic chart.

``` r
library(ape)
library(phylepic)

tree <- read.tree(text = "((D:0.3,C:0.4):0.5,B:0.1,A:0.2);")
metadata <- data.frame(
  ID = c("A", "B", "C", "D"),
  date = as.Date(c("2024-01-10", "2024-01-12", "2024-01-21", "2024-01-23")),
  country = factor(c("Australia", "Australia", NA, "New Zealand"))
)

phylepic(tree, metadata, ID, date) |> plot()
```

Refer to [the package
vignette](https://cidm-ph.github.io/phylepic/articles/phylepic.html) for
a more complete example.

## Citation

See `citation("phylepic")`.

Suster CJE, Watt AE, Wang Q, Chen SC, Kok J, Sintchenko V (2024).
“Combined visualization of genomic and epidemiological data for
outbreaks \[Preprint\].” *medRxiv*.
[doi:10.1101/2024.04.02.24305229](https://doi.org/10.1101/2024.04.02.24305229),
