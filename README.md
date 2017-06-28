# ssmTMB

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Travis-CI Build Status](https://travis-ci.org/ianjonsen/ssmTMB.svg?branch=master)](https://travis-ci.org/ianjonsen/ssmTMB)


**ssmTMB** - A fast state-space model for filtering Argos satellite tracking data

This is a minimal package that state-space filters and regularises to a specified time step error-prone Argos satellite tracking data. The fast estimation is acheived by using the Template Model Builder (TMB) package that provides C++ templates.

Read `?fit_ssm` for details and an example of how to use the package 

## Installation
First, ensure you have R version >= 3.3.0 installed:

```R
R.Version()
```

On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 

On Mac's, ensure you have installed [Xcode](https://developer.apple.com/xcode/) and Xcode developer tools. If installation is needed, make sure you start Xcode after install to ensure final setup of developer tools is completed. Both Xcode and Xcode developer tools can be installed from the [Mac App Store](https://itunes.apple.com/au/app/xcode/id497799835?mt=12)

Next, you will need to install `TMB` and it's dependencies:
```R
install.packages("TMB")
```

Install `devtools` and it's dependencies and then install `ssmTMB` from GitHub:

```R
install.packages("devtools")  
devtools::install_github("ianjonsen/ssmTMB")
```
