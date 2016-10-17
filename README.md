# ssmTMB

[![Travis-CI Build Status](https://travis-ci.org/ianjonsen/ssmTMB.svg?branch=master)](https://travis-ci.org/ianjonsen/ssmTMB)


**ssmTMB** - A fast state-space model for filtering Argos satellite tracking data

This is a minimal package that state-space filters and regularises to a specified time step error-prone Argos satellite tracking data. The fast estimation is acheived by using the Template Model Builder (TMB) package that provides C++ templates.

Read `?fit_ssm` for details and an example of how to use the package 

## Installation
First, ensure you have TMB and dependencies installed:

```R
library(TMB)
```

Get the development version of ssmTMB from GitHub:

```R
# install.packages("devtools")  
devtools::install_github("ianjonsen/ssmTMB")
```
