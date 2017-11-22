#' Activation Functions
#'
#' @param x numeric input vector
#' @param zero numerical zero value for threshold
#' @name act_fun
NULL

#' @export
#' @rdname act_fun
sigmoid <- function(x, zero = 1){
  1/(1+exp(- zero * x))
}

#' @export
#' @rdname act_fun
ReLU <- function(x, zero = 0){
  ifelse(x > zero, x, zero)
}

#' @export
#' @rdname act_fun
ReU <- function(x, zero = 0){
  ifelse(x > zero, x, -x)
}

#' @export
#' @rdname act_fun
th <- function(x, zero = 0){
  ifelse(x > zero, 1, 0)
}
