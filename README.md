
# Fiscal Impact Measure

[![standard-readme
compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)

<!-- badges: end -->

The Hutchins Center Fiscal Impact Measure shows how much local, state,
and federal tax and spending policy adds to or subtracts from overall
economic growth, and provides a near-term forecast of fiscal policies’
effects on economic activity.

Click
[here](https://www.brookings.edu/interactives/hutchins-center-fiscal-impact-measure/)
to see the most recent version of the FIM.

This repository contains the code used to calculate the FIM. It was originally authored by Manuel Alcala Kovalski and currently mantained by Nasiha Salwati. 

## Resources:

The FIM project is structured as an R package in order to easily load functions, data, and documentation. To understand the directory structure and conventions used throughout the projects and for an introduction to R packages take a look at the [Brookings Data Network Brownbag presentation on R packages](https://malcalakovalski.github.io/rpkgs/#1). For a deeper dive read Hadley Wickham and Jenny Bryan's book on [R packages](https://r-pkgs.org).

In addition, knowledge of Git and GitHub is important to collaborate on the FIM with other RA's and prevent things from breaking. To learn more you can get started with the [Brookings Data Network Brownbag on Git & GitHub](https://malcalakovalski.github.io/git_brownbag/#1).

## Installation
 
In order to run the FIM code you first need to install the FIM package by running 

``` r
# install.packages("devtools") (install devtools if you don't have it already)
devtools::install_github("Hutchins-Center/fim")
```

Once you finish installing it, you can clone this repository and run the code. 

## Instructions for BEA data release updates

### Starting an update

At the beginning of each FIM update you should create a new `branch` named `update-{month}-{year}` and do all your work there. This way if something goes wrong the `main` branch won't be affected. 

### Updating BEA data

The BEA releases their GDP estimates and revisions on a monthly basis. We pull these BEA updates from Haver by running the `data-raw/pre-process.R` script. That script saves the BEA data to analyze in the `data` folder. 

### Updating our forecast

Once the BEA data is read in you need to copy the data in `data/haver_pivoted.xlsx` to the `"haver pivoted"` sheet in `data/forecast.xlsx`. The next step does not involve R and consists of evaluating the forecast of each component in the spreadsheet. The spreadsheet contains detailed instructions for this step in its first sheet. 

Once the forecast is updated and approved you need to calculate the FIM.

### Calculating the FIM

In order to run the FIM calculations with the updated data run the `fiscal-impact.R` script found in the parent directory. This scripts merges the BEA data with our forecast, calculates the fiscal impact for purchases, taxes, and transfers at the federal and state level, and creates a parametrized RMarkdown report to compare results between one update to the next. 

All the relevant outputs of `fiscal-impact.R` can be found in the `results/{current-month}` folder, inclduing the RMarkdown update comparison report. A copy of this report is moved to the parent directory and renamed index.Rmd in order to share it quickly as a link using Github pages. 

### Checking results

You can check the changes between the most recent update and the previous one by opening the `index.html` file in the parent directory. This report creates comparisons for the level and contribution to GDP of each government purchase, tax, and transfer that we use. 

If after running this step there are no bugs and you are satisfied with the changes great. Often you might have an unexpected change or realize you want to make an adjustment so you might have to iterate the process of editing `data/forecast.xlsx` and running `fiscal-impact.R` a few more times.

### Completing an update

Once the adjustments to `data/forecast.xlsx` and the results are confirmed you need to merge the `update-{month}-{year}` folder you've been working on to the `main` branch. Make sure to write a meaningful commit message such as `"2021 Q4 update finalized"` to make it easy to browse the repository at the point in time in which that update was finalized. 




