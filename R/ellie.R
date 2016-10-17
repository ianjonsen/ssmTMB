#' Argos satellite track of a single southern elephant seal
#'
#' Data were sourced from the Integrated Marine Observing System (IMOS) - IMOS is
#' supported by the Australian Government through the National Collaborative Research
#' Infrastructure Strategy and the Super Science Initiative.
#'
#' @format A data frame with 405 rows and 5 variables:
#' \describe{
#'   \item{id}{the identifier unique to this individual seal}
#'   \item{date}{the POSIXt date in the GMT timezone of each observation}
#'   \item{lc}{the Argos location quality class}
#'   \item{lon}{the observed longitude}
#'   \item{lat}{the observed latitude}
#' }
#' @source \url{https://portal.aodn.org.au}
"ellie"
