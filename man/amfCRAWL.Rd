% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcrw.r
\name{amfCRAWL}
\alias{amfCRAWL}
\title{ARGOS Multiplication Factors}
\usage{
amfCRAWL()
}
\value{
A data.frame with columns
\item{\code{lc}}{ARGOS location class}
\item{\code{AMFlon}}{multiplication factor for longitude}
\item{\code{AMFlat}}{multiplication factor for latitude}
}
\description{
ARGOS Error Multiplication Factors for Location Classes
}
\details{
These are the error multiplication factors used to scale location
accuracy for each location class that are used in \pkg{crawl}.
}
