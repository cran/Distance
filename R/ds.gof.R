#' Goodness of fit tests for distance sampling models
#'
#' This function is deprecated, please see [`gof_ds`][gof_ds].
#'
#' @export
#' @param model deprecated.
#' @param breaks deprecated.
#' @param nc deprecated.
#' @param qq deprecated.
#' @param ks deprecated.
#' @param \dots deprecated.
#' @return Nothing, deprecated.
#' @author David L Miller
#' @seealso [`qqplot.ddf`][mrds::qqplot.ddf] [`ddf.gof`][mrds::ddf.gof]
#' @keywords utility
ds.gof <- function(model, breaks=NULL, nc=NULL, qq=TRUE, ks=FALSE, ...){
  stop("ds.gof is deprected, please use gof_ds")
}
