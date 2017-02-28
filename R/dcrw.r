##' Correlated Random Walk Filter
##'
##' Fit a correlated random walk to filter a track and predict
##' locations on a regular time step.
##'
##' The input track is given as a dataframe where each row is an
##' observed location, with columns
##' \describe{
##' \item{'date'}{observation time (as GMT POSIXct),}
##' \item{'lon'}{observed longitude,}
##' \item{'lat'}{observed latitude,}
##' \item{'lc'}{location class.}
##' }
##'
##' The TMB parameter list can be specified directly with the
##' \code{parameters} argument. Otherwise suitable parameter values
##' are estimated from predicted locations estimated by fitting loess
##' smooths to the raw observations.
##'
##' The filtering model assumes the errors in longitude and latitude
##' are proportional to scale factors determined by the location
##' class. The scale factors are specified through the \code{amf}
##' argument. By default the function uses the same scaling factors
##' for location accuracy as used in the R package crawl for ARGOS data.
##'
##' @title Correlated Random Walk Filter
##' @param d a data frame of observations
##' @param subset a logical vector indicating the subset of data records to be filtered
##' @param tstep the time step to predict to (in days)
##' @param nu degrees of freedom parameter
##' @param gamma the autocorrelation parameter used to estimate
##'   initial parameters
##' @param parameters the TMB parameter list
##' @param optim numerical optimizer
##' @param verbose report progress during minimization
##' @param amf ARGOS error multiplication factors
##' @param span the span parameter for the loess fits used to estimate
##'   initial locations
##' @return a list with components
##' \item{\code{predicted}}{a data.frame of predicted location states}
##' \item{\code{fitted}}{a data.frame of fitted locations}
##' \item{\code{par}}{model parameter summmary}
##' \item{\code{data}}{the input data.frame}
##' \item{\code{subset}}{the input subset vector}
##'\item{\code{tstep}}{the prediction time step}
##' \item{\code{opt}}{the object returned by the optimizer}
##' \item{\code{tmb}}{the TMB object}
##' \item{\code{aic}}{the calculated Akaike Information Criterion}
##'
##' @examples
#'
#' # Fit DCRW model for state filtering and regularization
#' data(ellie)
#' fit <- fit_ssm(ellie, tstep = 6 / 24)
#'
#' ## plot predicted track over observations
#' plot(lat ~ lon, fit$data, pch = 3, col = "firebrick")
#' points(lat ~ lon, fit$predicted, pch = 19, cex = 0.5, col = "dodgerblue")
#'
#' ## plot residuals for longitude and latitude
#' layout(matrix(1:2, 2, 1))
#' lon.res <- fit$data$lon - fit$fitted$lon
#' lat.res <- fit$data$lat - fit$fitted$lat
#' plot(fit$data$date, lon.res, xlab = "")
#' abline(h = 0, lty = 2, col = "red")
#' plot(fit$data$date, lat.res, xlab = "date")
#' abline(h = 0, lty = 2, col = "red")
#'
##'
##' @useDynLib ssmTMB
##' @importFrom TMB MakeADFun sdreport
##' @importFrom stats loess loess.control cov sd predict nlminb
##' @export
fit_ssm <-
  function(d,
           subset = rep(TRUE, nrow(d)),
           tstep = 2.4 / 24,
           nu = 5,
           gamma = 0.5,
           parameters = NULL,
           optim = c("nlminb", "optim"),
           verbose = FALSE,
           amf = amfCRAWL(),
           span = 0.3) {
    optim <- match.arg(optim)
    d <- d[subset,]

    ## Ensure POSIXct dates
    d$date <- as.POSIXct(d$date, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    d <- d[order(d$date),]

    ## pre-process ARGOS data to obtain interpolation indices & weights generate data list
    tmb <- argos2tmb(d, tstep = tstep, amf = amf)

    ## Predict track from loess smooths
    fit.lon <-
      loess(
        lon ~ as.numeric(date),
        data = d,
        span = span,
        na.action = "na.exclude",
        control = loess.control(surface = "direct")
      )
    fit.lat <-
      loess(
        lat ~ as.numeric(date),
        data = d,
        span = span,
        na.action = "na.exclude",
        control = loess.control(surface = "direct")
      )

    ## Predict track, increments and stochastic innovations
    xs <-
      cbind(predict(fit.lon, newdata = data.frame(date = as.numeric(tmb$ts))),
            predict(fit.lat, newdata = data.frame(date = as.numeric(tmb$ts))))

    ## TMB - create parameter list
    if (is.null(parameters)) {
      ## Estimate increments and stochastic innovations
      ds <- xs[-1,] - xs[-nrow(xs),]
      es <- ds[-1,] - gamma * ds[-nrow(ds),]

      ## Estimate components of variance
      V <- cov(es)
      sigma <- sqrt(diag(V))
      rho <- V[1, 2] / prod(sqrt(diag(V)))
      tau <-
        c(sd(fit.lon$residuals / tmb$K[, 1]),
          sd(fit.lat$residuals / tmb$K[, 2]))

      parameters <-
        list(
          theta = 0,
          l_gamma = log(gamma / (1 - gamma)),
          l_sigma = log(pmax(1e-08, sigma)),
          l_rho = log((1 + rho) / (1 - rho)),
          l_tau = log(pmax(1e-08,
                           tau)),
          x = xs
        )
    }

    ## TMB - data list
    data <-
      list(
        y = tmb$y,
        idx = tmb$idx,
        w = tmb$w,
        K = tmb$K,
        nu = nu,
        ts = tstep
      )

    ## TMB - create objective function
    obj <-
      TMB::MakeADFun(
        data,
        parameters,
        random = "x",
        DLL = "ssmTMB",
        silent = !verbose
      )
    obj$env$inner.control$trace <- verbose
    obj$env$tracemgc <- verbose

    ## Minimize objective function
    opt <-
      suppressWarnings(switch(
        optim,
        nlminb = nlminb(obj$par, obj$fn, obj$gr),
        optim = do.call("optim", obj)
      ))

    ## Parameters, states and the fitted values
    rep <- TMB::sdreport(obj)
    fxd <- summary(rep, "report")

    rdm <-
      matrix(summary(rep, "random"),
             length(tmb$ts),
             4,
             dimnames = list(NULL, c("lon", "lat", "lon.se", "lat.se")))
    ftd <-
      tmb$ws * rdm[tmb$idx + 1, 1:2] + (1 - tmb$ws) * rdm[tmb$idx + 2, 1:2]

    if (optim == "nlminb")
      aic <- 2 * length(opt[["par"]]) + 2 * opt[["objective"]]

    list(
      predicted = cbind(date = tmb$ts, as.data.frame(rdm)),
      fitted = cbind(date = d$date, as.data.frame(ftd)),
      par = fxd,
      data = d,
      subset = subset,
      tstep = tstep,
      opt = opt,
      tmb = obj,
      aic = aic
    )
  }





##' Format ARGOS track data for filtering
##'
##' This is an internal function used by \code{fit_ssm} to format track
##' data for the TMB filter.
##'
##' The input track is given as a data.frame where each row is an
##' observed location and columns
##' \describe{
##' \item{'date'}{observation time (POSIXct,GMT),}
##' \item{'lon'}{observed longitude,}
##' \item{'lat'}{observed latitude,}
##' \item{'lc'}{ARGOS location class.}
##' }
##'
##' From this it calculates interpolation indices \code{idx} and
##' weights \code{ws} such that if \code{x} is the matrix of predicted
##' states, the fitted locations are \code{ws*x[idx+1,] +
##' (1-ws)*x[idx+2,]}.
##'
##' By default the function uses the same ARGOS multiplication factors
##' to scale location accuracy by location class as used in
##' \pkg{crawl}.
##'
##' @title ARGOS data pre-processing
##' @param d a data frame of observations (see details)
##' @param tstep the time step to predict to (in days)
##' @param extrap if TRUE, the final predicted state occurs
##'   immediately before the last observation, otherwise the final
##'   predicted state occurs immediately after the last observation.
##' @param amf Argos error scale mmultiplication factors
##' @return A list with components
##' \item{\code{y}}{a 2 column matrix of the lon,lat observations}
##' \item{\code{K}}{a 2 column matrix of the ARGOS scale factors}
##' \item{\code{idx}}{a vector of interpolation indices}
##' \item{\code{ws}}{a vector of interpolation weights}
##' \item{\code{ts}}{the times at which states are predicted (POSIXct,GMT)}
##' \item{\code{dt}}{the time step at which states are predicted (secs)}
##' @export
argos2tmb <-
  function(d,
           tstep = 1,
           extrap = FALSE,
           amf = amfCRAWL()) {
    ## Check ARGOS location accuracies
    d$lc <-
      factor(d$lc,
             levels = c("3", "2", "1", "0", "A", "B"),
             ordered = TRUE)

    ## Merge ARGOS error multiplication factors
    d <- merge(d, amf, by = "lc", all.x = TRUE)
    d <- d[order(d$date),]

    ## Interpolation indices and weights
    dt <- tstep * 86400
    tms <- (as.numeric(d$date) - as.numeric(d$date[1])) / dt
    index <- floor(tms)
    if (extrap)
      index <- pmax(index, max(index) - 1)
    weights <- 1 - (tms - index)

    list(
      y = cbind(d$lon, d$lat),
      K = cbind(d$AMFlon, d$AMFlat),
      idx = index,
      ws = weights,
      ts = seq(d$date[1], by = dt, length.out = max(index) + 2),
      dt = dt
    )
  }


##' ARGOS Error Multiplication Factors for Location Classes
##'
##' These are the error multiplication factors used to scale location
##' accuracy for each location class that are used in \pkg{crawl}.
##' @title ARGOS Multiplication Factors
##' @return A data.frame with columns
##' \item{\code{lc}}{ARGOS location class}
##' \item{\code{AMFlon}}{multiplication factor for longitude}
##' \item{\code{AMFlat}}{multiplication factor for latitude}
##' @export
amfCRAWL <- function() {
  data.frame(
    lc = factor(
      c("3", "2", "1", "0", "A", "B"),
      levels = c("3", "2", "1", "0", "A", "B"),
      ordered = TRUE
    ),
    AMFlon = c(1, 1.54, 3.72, 23.9, 13.51, 44.22),
    AMFlat = c(1, 1.29, 2.55, 103.7, 14.99, 32.53)
  )
}
