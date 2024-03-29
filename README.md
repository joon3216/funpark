
<!-- README.md is generated from README.Rmd. Please edit that file -->
funpark
=======

<!-- badges: start -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![version](https://img.shields.io/badge/version-0.2.6-blue.svg?cacheSeconds=2592000)
<!-- badges: end -->
The package is for archiving those functions in [Research materials](https://joon3216.github.io/research_materials) section.

Installation
------------

The package is not uploaded on CRAN. You can download `funpark` package by:

``` r
# install.packages("devtools")
devtools::install_github("joon3216/funpark")
```

Examples
--------

There are two themes involved in this package:

### Response column(s) reshaping

See examples of:

-   reshaping:
    -   [`binarize_binom()`](https://joon3216.github.io/research_materials/2018/binarize#case-1-nodal)
    -   [`binarize_pois()`](https://joon3216.github.io/research_materials/2018/binarize#case-2-femsmoke)
    -   [`change_form()`](https://joon3216.github.io/research_materials/2018/binarize#aside-change_form)
-   applications regarding ROC curves:
    -   [`plot_roc()`](https://joon3216.github.io/research_materials/2018/binarize#roc-curve)
    -   [`CI_auc()`](https://joon3216.github.io/research_materials/2018/binarize#confidence-interval-for-auc)

### Statistical computations

-   [`fusion_estimates()`](https://joon3216.github.io/research_materials/2018/non_separable_penalty#the-data), and searching for the best tuning parameter using [k-fold cross-validation](https://joon3216.github.io/research_materials/2019/cross_validation_fs)
-   [using pgf and DFT](https://joon3216.github.io/research_materials/2018/pgf) to evaluate complicated pmfs
-   imputing missing numerical data using the [EM algorithm](https://joon3216.github.io/research_materials/2019/em_imputation)
