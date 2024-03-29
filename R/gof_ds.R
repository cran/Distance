#' Goodness of fit testing and quantile-quantile plots
#'
#' Goodness of fit testing for detection function models. For continuous
#' distances Kolmogorov-Smirnov and Cramer-von Mises tests can be used, when
#' binned or continuous distances are used a \eqn{\chi^2} test can be used.
#'
#' Kolmogorov-Smirnov and Cramer-von Mises tests are based on looking at the
#' quantile-quantile plot produced by [`qqplot.ddf`][mrds::qqplot.ddf] and
#' deviations from the line \eqn{x=y}.
#'
#' The Kolmogorov-Smirnov test asks the question "what's the largest vertical
#' distance between a point and the \eqn{y=x} line?" It uses this distance as a
#' statistic to test the null hypothesis that the samples (EDF and CDF in our
#' case) are from the same distribution (and hence our model fits well). If the
#' deviation between the \eqn{y=x} line and the points is too large we reject
#' the null hypothesis and say the model doesn't have a good fit.
#'
#' Rather than looking at the single biggest difference between the y=x line
#' and the points in the Q-Q plot, we might prefer to think about all the
#' differences between line and points, since there may be many smaller
#' differences that we want to take into account rather than looking for one
#' large deviation. Its null hypothesis is the same, but the statistic it uses
#' is the sum of the deviations from each of the point to the line.
#'
#' A chi-squared test is also run if `chisq=TRUE`. In this case binning of
#' distances is required if distance data are continuous. This can be specified
#' as a number of equally-spaced bins (using the argument `nc=`) or the
#' cutpoints of bins (using `breaks=`). The test compares the number of
#' observations in a given bin to the number predicted under the fitted
#' detection function.
#'
#' @section Details:
#'
#' Note that a bootstrap procedure is required for the Kolmogorov-Smirnov test
#' to ensure that the p-values from the procedure are correct as the we are
#' comparing the cumulative distribution function (CDF) and empirical
#' distribution function (EDF) and we have estimated the parameters of the
#' detection function. The `nboot` parameter controls the number of bootstraps
#' to use. Set to `0` to avoid computing bootstraps (much faster but with no
#' Kolmogorov-Smirnov results, of course).
#'
#' @param model a fitted detection function.
#' @param plot if `TRUE` the Q-Q plot is plotted
#' @param chisq if `TRUE` then chi-squared statistic is calculated even
#' for models that use exact distances. Ignored for models that use binned
#' distances
#' @param nboot number of replicates to use to calculate p-values for the
#' Kolmogorov-Smirnov goodness of fit test statistics
#' @param ks perform the Kolmogorov-Smirnov test (this involves many bootstraps
#' so can take a while)
#' @param nc number of evenly-spaced distance classes for chi-squared test, if
#' `chisq=TRUE`
#' @param breaks vector of cutpoints to use for binning, if `chisq=TRUE`
#' @param ... other arguments to be passed to [`ddf.gof`][mrds::ddf.gof]
#' @export
#' @examples
#' \dontrun{
#' # fit and test a simple model for the golf tee data
#' library(Distance)
#' data(book.tee.data)
#' tee.data <- subset(book.tee.data$book.tee.dataframe, observer==1)
#' ds.model <- ds(tee.data,4)
#' # don't make plot
#' gof_ds(ds.model, plot=FALSE)
#'}
gof_ds <- function(model, plot=TRUE, chisq=FALSE, nboot=100, ks=FALSE,
                   nc=NULL, breaks=NULL, ...){

  gof <- suppressMessages(ddf.gof(model$ddf, qq=plot, nboot=nboot, ks=ks, nc=nc,
                                  breaks=breaks, ...))

  if(model$ddf$meta.data$binned | chisq){
    return(gof)
  }else{
    gof$chisquare <- NULL
    return(gof)
  }
}
