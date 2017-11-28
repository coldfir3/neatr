#' Builds a Neural Network object
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
    weight_range <- rep(weight_range, 2) * c(-1,1)
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
  ) %>% round(3)

  if(all(hiddens == 0))
    number <- c(
      0,
      1:inputs,
      1:outputs
    )
  else if(any(hiddens == 0))
    stop('all hiddens must be different of 0')
  else
    number <- c(
      0,
      1:inputs,
      1:outputs,
      (hiddens %>% map(function(x) seq(from = 1, to = x, by = 1)) %>% unlist)
    )

  nodes <- data.frame(
    type = type,
    number = number,
    layer = layer
  )

  links <- get_possible_links(nodes)

  if(is.null(weights))
    links$weights <- stats::runif(nrow(links), weight_range[1], weight_range[2])
  else if(nrow(links) > length(weights)){
    length(weights) <- nrow(links)
    links$weights[which(is.na(weights))] <- stats::runif(sum(is.na(weights)), weight_range[1], weight_range[2])
  }
  else if(nrow(links) < length(weights)){
    links$wegiths <- wegiths[1:nrow(links)]
  }

  links$enabled <- stats::runif(nrow(links)) < enable_rate

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
      sum(nn$nodes$type == 'bias'), ' bias, ',
      sum(nn$nodes$type == 'hidden'), ' hid, ',
      sum(nn$nodes$type == 'output'), ' out); ',
      nrow(nn$links), ' links (',
      sum(nn$links$enable), ' enabled, newest gene: genotype ',
#      max(nn$links$number),
      ')\n',
      sep = '')
}

#' @export
c.nn <- function(...){

  nn_pop <- (c(list(...)))
  class(nn_pop) <- 'nnpop'

  nn_pop
}

#' @export
plot.nn <- function(x, y, ...){

  nn <- x

  nn$links$enabled <- factor(1 - as.integer(nn$links$enabled), levels = c(0,1), labels = c(TRUE, FALSE))

  tab <- stats::aggregate(nn$nodes$number, list(nn$nodes$layer), mean) %>% cbind(table(nn$nodes$layer))

  mu <- tab %>% with(rep(x, Freq))
  mu[order(nn$nodes$layer)] <- mu
  mu.in <- tab$x[match(nn$links$input.layer, tab$Group.1)]
  mu.out <- tab$x[match(nn$links$output.layer, tab$Group.1)]


  nn$nodes$posi <- nn$nodes$number - mu
  nn$links$input.number <- nn$links$input.number - mu.in
  nn$links$output.number <- nn$links$output.number - mu.out

  # plot the nodes
  p <- ggplot2::ggplot(nn$nodes, ggplot2::aes_string(x = 'layer', y = 'posi', shape = 'type')) +
    ggplot2::geom_point(size = 3)

  # plot the links
  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes_string(
        x = 'input.layer',
        y = 'input.number',
        xend = 'output.layer',
        yend = 'output.number',
        col = 'weights',
        lty = 'enabled'),
      data = nn$links,
      inherit.aes = FALSE, alpha = 0.5, lwd = 1.2)

  # set the theme
  p +
    ggplot2::scale_shape_manual('Type', values = c(7,2,6,1)) +
    #viridis::scale_color_viridis('Weight') +
    ggplot2::scale_colour_gradient2('Weight') +
    ggplot2::theme_void() +
    ggplot2::scale_linetype_manual('Enabled', values = c(1,0))

}

#' @export
'==.nn' <- function(nna, nnb){
  if(any(dim(nna$nodes) != dim(nnb$nodes)))
    return(FALSE)
  if(any(dim(nna$links) != dim(nnb$links)))
    return(FALSE)
  nodes_equal <- all(nna$nodes == nnb$nodes)
  links_equal <- all(nna$links == nnb$links)
  nodes_equal & links_equal
}

LINKS_STRUCTURE = c('input.number', 'input.layer', 'output.number', 'output.layer')
