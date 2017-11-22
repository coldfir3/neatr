#' Activates a neural nnwork
#'
#' @param input Numerical vector of the same length of the input neurons of the net
#' @param nn Neural Network
#' @param act_fun activation function used
#'
#' @return Numerical Vector of the output for the Neural Network
#'
#' @export
#' @examples
#' build.nn(2, 1, enable_rate = 1) %>% activate(c(1,1), .)
#' build.nn(2, 1, 2, enable_rate = 1) %>% activate(c(1,1), .)
#' build.nn(2, 1, c(2,3), enable_rate = 1) %>% activate(c(1,1), .)
#' build.nn(2, 2, c(5,5,5), enable_rate = 1) %>% activate(c(1,1), .)
activate <- function(input, nn, act_fun = sigmoid){

  if(length(input) != sum(nn$nodes$type == 'input'))
    stop('length of input must be equal to number of imputs')

  nn$nodes$value <- NA
  nn$nodes$value[which(nn$nodes$type == 'bias')] <- 1
  nn$nodes$value[which(nn$nodes$type == 'input')] <- input

  nodes_tbd <- order(nn$nodes$layer)[nn$nodes$layer != 0]
  for(node in nodes_tbd){
    links <- nn$links[nn$links$output == nn$nodes$number[node],]
    values <- nn$nodes$value[nn$nodes$number %in% links$input]
    weights <- links$weights
    enables <- links$enabled
    nn$nodes$value[node] <- act_fun(sum(values * weights * enables))
  }

  nn$nodes$value[nn$nodes$type == 'output']
}
