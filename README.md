
<!-- README.md is generated from README.Rmd. Please edit that file -->
# Fiscal Impact Measure
[![standard-readme compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)

[![Travis build
status](https://travis-ci.com/malcalakovalski/fim-pkg.svg?branch=master)](https://travis-ci.com/malcalakovalski/fim-pkg)
<!-- badges: end -->

The Fiscal Impact Measure (FIM) is a tool created the Hutchins Center to
illustrate how much local, state and federal fiscal policy adds to or
subtracts from overall economic growth. When the FIM is positive, policy
is expansionary in the sense that it is pushing growth in real Gross
Domestic Product (GDP) above its longer-run potential. When the FIM is
negative, policy is lowering real GDP growth relative to potential. The
FIM is broader than measures of fiscal impetus that rely on the size of
the federal deficit because it includes the includes the direct effects
of federal, state, and local government purchases as well as the more
indirect effects of government taxes and government transfers, which
affect private consumption.

## Installation

You can install the released version of fim from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fim")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("malcalakovalski/fim")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fim)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
