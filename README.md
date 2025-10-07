
# AdvancedRHT2025Lab06

<!-- badges: start -->
[![R-CMD-check](https://github.com/Cl4ryty/AdvancedRHT2025Lab06/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Cl4ryty/AdvancedRHT2025Lab06/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of AdvancedRHT2025Lab06 is to fulfill the requirements of lab 6 of the Advanced Programming in R course. 
Specifically, this includes multiple solutions for solving the knapsack problem. 

## Installation

You can install the development version of AdvancedRHT2025Lab06 like so:

``` r
devtools::install_github("https://github.com/Cl4ryty/AdvancedRHT2025Lab05", build_vignettes=TRUE)
```

## Example

This is a basic example:

``` r
library(AdvancedRHT2025Lab06)
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 1000000
knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
                               v = runif(n = n, 0, 10000))
brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
```

