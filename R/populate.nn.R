#' Build a population of Neural Nets
#'
#' This function generates a population of neural nets with the same input-output configuration
#'
#' @param npop Numerical value of the number of individuals of the population
#' @inheritParams build.nn
#'
#' @export
#' @examples
#' populate.nn(10, 3, 1)
#' populate.nn(100, 3, 1)
populate.nn <- function(npop, inputs, outputs, hiddens = 0, weights = NULL, weight_range = c(-1,1), enable_rate = 0.7){

  nn_pop <- replicate(npop, build.nn(inputs, outputs, hiddens,
                                     weight_range = weight_range,
                                     enable_rate = enable_rate), simplify = FALSE)
  class(nn_pop) <- 'nnpop'

  nn_pop
}
setOldClass("nnpop")

#' @export
print.nnpop <- function(x, ...){

  nn_pop <- x

  cat('Population of ', length(nn_pop), 'Neural Networks.\n')
  cat('Individual configuration:\n')

  N <- length(nn_pop)
  if(N < 8)
    N <- 1:N
  else
    N <- c(1:3, NA, N - 2:0)
  for(n in N){
    if(is.na(n))
      cat('  ...\n')
    else{
      cat('  ', sprintf('%03d', n), ': ', sep = '')
      print(nn_pop[[n]])
    }
  }
}

#' @export
c.nnpop <- function(...){

  nn_pop <- purrr::flatten(c(list(...)))
  class(nn_pop) <- 'nnpop'

  nn_pop
}
