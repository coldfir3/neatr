#' Builds an Neural Network object
#'
#' @param inputs,outputs Numeric indicating the number of inputs and outputs
#' @param hiddens Numeric Vector indicating the number of hidden neurons per layer (optional)
#' @param weights Numeric Vector of the nodal weight, if NULL (default) random weights
#'  will be sampled from an uniform distribution over weight_range.
#'  missing values will be randomized and excess values will be cropped
#' @param weight_range Numeric Vector of length 2 with the lower and
#'  upper bounds for the random weight generation
#' @param enable_rate Numeric between 0 and 1 indicating the probability that a link will be enabled
#'
#' @return neural network object
#'
#' @export
#' @examples
#' build.nn(2, 1, enable_rate = 1) %>% plot
#' build.nn(2, 1, 2, enable_rate = 1) %>% plot
#' build.nn(2, 1, c(2,3), enable_rate = 1, weight_range = 0) %>% plot
#' build.nn(2, 2, c(5,5,5), enable_rate = 1, weight_range = 0) %>% plot
build.nn <- function(inputs, outputs, hiddens = 0, weights = NULL, weight_range = c(-1,1), enable_rate = 0.7){

  if(inputs < 1)
    stop('network must have at least 1 input')

  if(outputs < 1)
    stop('network must have at least 1 output')

  if(length(weight_range) == 1)
    weight_range <- rep(weight_range, 2)
  else if(length(weight_range) != 2)
    stop('weight_range must be of lenght 1 or 2')

  ### generate the nodes

  type <- factor(c(
    0,
    rep(1, inputs),
    rep(2, outputs),
    rep(3, sum(hiddens))
  ), levels = c(0,1,2,3), labels = c('bias','input', 'output', 'hidden'))

  layer <- c(
    rep(0, inputs + 1),
    rep(1, outputs),
    rep(1:length(hiddens)/(length(hiddens)+1), hiddens)
  )

  number <- 0:(length(type) - 1)

  nodes <- data.frame(
    number = number,
    type = type,
    layer = layer
  )

  links <- rbind(
    data.frame(expand.grid(
      input = number[nodes$type == 'input'],
      output = number[nodes$type == 'output']
    )),
    data.frame(expand.grid(
      input = number[nodes$type == 'hidden'],
      output = number[nodes$type == 'output']
    )),
    data.frame(expand.grid(
      input = number[nodes$type == 'input'],
      output = number[nodes$type == 'hidden']
    ))
  )

  number <- 1:nrow(links)

  if(is.null(weights))
    weights <- stats::runif(number, weight_range[1], weight_range[2])
  else if(number > length(weights)){
    length(weights) <- number
    weights[which(is.na(weights))] <- stats::runif(sum(is.na(weights)), weight_range[1], weight_range[2])
  }
  else if(number < length(weights)){
    wegiths <- wegiths[1:number]
  }

  enabled <- stats::runif(number) < enable_rate

  links <- cbind(
    number,
    links,
    weights,
    enabled
  )

  nn <- list(
    nodes = nodes,
    links = links
  )
  class(nn) <- 'nn'

  nn
}
setOldClass("nn")

#' @export
print.nn <- function(x, ...){

  nn <- x

  cat('Neural Network: ',
      nrow(nn$nodes), ' nodes (',
      sum(nn$nodes$type == 'input'), ' in, ',
      sum(nn$nodes$type == 'hidden'), ' hid, ',
      sum(nn$nodes$type == 'output'), ' out); ',
      nrow(nn$links), ' links (',
      sum(nn$links$enable), ' enabled, newest gene: genotype ',
      max(nn$links$number), ')\n',
      sep = '')
}

#' @export
c.nn <- function(...){

  nn_pop <- purrr::flatten(c(list(...)))
  class(nn_pop) <- 'nnpop'

  nn_pop
}

#' @export
plot.nn <- function(x, y, ...){ ### FIX NEEDED: change this to simply plot and create an object of class S3 for nn

  nn <- x

  layer <- nn$nodes$layer
  layer[order(layer)] <- rep(0:(length(unique(layer))-1), table(layer))
  layer -> nn$nodes$layer

  ord <- 1:nrow(nn$nodes)
  nn$nodes <- nn$nodes[order(nn$nodes$layer),]
  posi <- NULL
  for(p in unique(nn$nodes$layer))
    posi <- c(posi, 1:table(nn$nodes$layer)[p+1] - mean(1:table(nn$nodes$layer)[p+1]))
  nn$nodes$posi <- posi
  nn$nodes <- nn$nodes[order(ord),]

  nn$links$enable <- factor(1 - as.integer(nn$links$enable), levels = c(0,1), labels = c(TRUE, FALSE))

  p <- ggplot2::ggplot(nn$nodes, ggplot2::aes_string(x = 'layer', y = 'posi', shape = 'type')) +
    ggplot2::geom_segment(ggplot2::aes_string(x = 'In.layer', y = 'In.posi', xend = 'Out.layer', yend = 'Out.posi', col = 'w', lty = 'on'),
                          data = cbind(In = nn$nodes[nn$links$input %>% map(`==`, nn$nodes$number) %>% map_dbl(which), c('layer', 'posi')],
                                       Out = nn$nodes[nn$links$output %>% map(`==`, nn$nodes$number) %>% map_dbl(which), c('layer', 'posi')],
                                       w = nn$links$weight,
                                       on = nn$links$enable),
                          inherit.aes = FALSE, alpha = 0.5, lwd = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_shape_manual('Type', values = c(7,2,6,1)) +
    viridis::scale_color_viridis('Weight') + ggplot2::theme_void() +
    ggplot2::scale_linetype_manual('Enabled', values = c(1,0))

  p
}

