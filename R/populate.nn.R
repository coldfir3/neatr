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

#' @export
subset.nnpop <- function(x, subset, ...){

  population <- x[subset]
  class(population) <- 'nnpop'

  population
}

#' @export
mean.nnpop <- function(x, trim = 0, na.rm = FALSE, ...){

  population <- x
  gene_pool <- neatr:::build_gene_pool(population)

  weights <-
    population %>%
    map(neatr:::sequentiate, gene_pool, zero2na = FALSE, na.rm = TRUE) %>%
    do.call(rbind,.) %>%
    apply(2, mean, na.rm = TRUE)

  gene_pool$links$weights <- weights
  gene_pool
}

#' @export
median_nnpop <- function(x, trim = 0, na.rm = FALSE, ...){ #make this generic for stats::median

  population <- x
  gene_pool <- neatr:::build_gene_pool(population)

  weights <-
    population %>%
    map(neatr:::sequentiate, gene_pool, zero2na = FALSE, na.rm = TRUE) %>%
    do.call(rbind,.) %>%
    apply(2, median, na.rm = TRUE)

  gene_pool$links$weights <- weights
  gene_pool
}

#' @export
sd.nnpop <- function(x, trim = 0, na.rm = FALSE, ...){

  population <- x
  gene_pool <- neatr:::build_gene_pool(population)

  weights <-
    population %>%
    map(neatr:::sequentiate, gene_pool, zero2na = FALSE, na.rm = TRUE) %>%
    do.call(rbind,.) %>%
    apply(2, sd, na.rm = TRUE)

  gene_pool$links$weights <- weights
  gene_pool
}


#' @export
plot.nnpop <- function(x, y, ..., animate = FALSE){

  nnpop <- x
  names(nnpop) <- 1:length(nnpop)

  center <- function(nn){
    tab <- stats::aggregate(nn$nodes$number, list(nn$nodes$layer), mean) %>% cbind(table(nn$nodes$layer))

    mu <- tab %>% with(rep(x, Freq))
    mu[order(nn$nodes$layer)] <- mu
    mu.in <- tab$x[match(nn$links$input.layer, tab$Group.1)]
    mu.out <- tab$x[match(nn$links$output.layer, tab$Group.1)]

    nn$nodes$posi <- nn$nodes$number - mu
    nn$links$input.number <- nn$links$input.number - mu.in
    nn$links$output.number <- nn$links$output.number - mu.out

    class(nn) <- 'nn'
    nn
  }

  nnpop <- nnpop %>% map(center)

  links <- nnpop %>% map('links') %>% dplyr::bind_rows(.id = "id")
  nodes <- nnpop %>% map('nodes') %>% dplyr::bind_rows(.id = "id")

  storage.mode(links$id) <- 'numeric'
  storage.mode(nodes$id) <- 'numeric'
  links$enabled <- factor(1 - as.integer(links$enabled), levels = c(0,1), labels = c(TRUE, FALSE))

  # plot the nodes
  if(animate)
    p <- ggplot2::ggplot(nodes, ggplot2::aes_string(x = 'layer', y = 'posi', shape = 'type')) +
      ggplot2::geom_point(size = 3, ggplot2::aes_string(frame = 'id'))
  else
    p <- ggplot2::ggplot(nodes, ggplot2::aes_string(x = 'layer', y = 'posi', shape = 'type')) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~id)

  # plot the links
  if(animate)
  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes_string(
        x = 'input.layer',
        y = 'input.number',
        xend = 'output.layer',
        yend = 'output.number',
        col = 'weights',
        lty = 'enabled',
        frame = 'id'),
      data = links,
      inherit.aes = FALSE, alpha = 0.5, lwd = 1.2)
  else
    p <- p +
    ggplot2::geom_segment(
      ggplot2::aes_string(
        x = 'input.layer',
        y = 'input.number',
        xend = 'output.layer',
        yend = 'output.number',
        col = 'weights',
        lty = 'enabled'),
      data = links,
      inherit.aes = FALSE, alpha = 0.5, lwd = 1.2)

  # set the theme
  p <- p +
    ggplot2::scale_shape_manual('Type', values = c(7,2,6,1)) +
    ggplot2::scale_colour_gradient2('Weight') +
    ggplot2::theme_void() +
    ggplot2::scale_linetype_manual('Enabled', values = c(1,0))

  p

}

