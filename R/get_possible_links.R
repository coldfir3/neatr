get_possible_links <- function(nodes){

  numbers <- nodes %>% split(nodes$layer) %>% map('number')
  levels_idx <- utils::combn(1:length(numbers), 2) %>% as.data.frame %>% as.list

  links <- levels_idx %>% map(function(idx){
    x <- expand.grid(numbers[idx])
    x <- cbind(x[1], as.numeric(names(numbers))[idx[1]], x[2], as.numeric(names(numbers))[idx[2]])
    names(x) <- LINKS_STRUCTURE
    x
  }) %>% do.call(rbind, .)

  number <- 1:nrow(links)
  weights <- 0
  enabled <- TRUE

  links <- cbind(
    links,
    weights,
    enabled
  )

  row.names(links) <- NULL

  links
}
