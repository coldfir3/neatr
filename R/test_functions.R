#' Test function: XOR netowrk
#'
#' bla bla bla
#'
#' @param nn a neural netowrk
#' @examples
#' nn <- build.nn(2, 1)
#' xor.nn(nn)
#' @export
xor.nn <- function(nn){
  if(!exists('fn_count'))
    fn_count <- 0
  fn_count <<- fn_count + 1

  inputs <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE))
  true_outputs <- xor(inputs[,1], inputs[,2])
  nn_outputs <- apply(inputs, 1, activate, nn = nn, act_fun = function(x) sigmoid(x, 4.9))

  # compute the accuracy (maxium score of 16)
  (4 - sum(abs(true_outputs - nn_outputs)))^2
}
