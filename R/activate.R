#' Activates a neural network
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

  get_values <- function(input.number, input.layer, weights, enabled, ...){
    nodes[nodes$number == input.number & nodes$layer == input.layer, 'value'] * weights * enabled
  }

  nodes <- nn$nodes
  links <- nn$links

  nodes$value <- NA

  nodes$value[nodes$type == 'input'] <- input
  nodes$value[nodes$type == 'bias'] <- 1

  layers <- sort(unique(nodes$layer))

  for(layer in layers[-1]){
    numbers <- nodes$number[nodes$layer == layer]
    for(number in numbers){
      active_links <- links[links$output.number == number & links$output.layer ==layer, ]
      nodes$value[nodes$layer == layer & nodes$number == number] <-
        active_links %>% pmap(get_values) %>% unlist %>% sum %>% act_fun
    }
  }

  res <- nodes$value[nodes$type == 'output']

  return(res)

}
