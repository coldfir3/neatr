#' neatr: blablabla
#'
#' @import purrr
#'
#' @docType package
#' @name neatr
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @useDynLib neatr
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("neatr", libpath)
}
