#' @examples
#'
#' nn_pop <- populate.nn(10, 2, 1)
#' neatr:::init_gp(nn_pop[[1]])
#' for(i in 1:5)
#'   nn_pop[[i]] <- neatr:::mutate(nn_pop[[i]], em_rate = 1, wm_rate = 1, lm_rate = 1, nm_rate = 1, wm_power = 1, wm_limit = 10)
#' species <- neatr:::speciate_kmeans(nn_pop, n_species = 2)
#' for(i in 1:5)
#'   nn_pop[[i]] <- neatr:::mutate(nn_pop[[i]], em_rate = 1, wm_rate = 1, lm_rate = 1, nm_rate = 1, wm_power = 1, wm_limit = 10)
#' species <- neatr:::speciate_kmeans(nn_pop, nn_reference = species$nn_reference)
speciate_kmeans <- function(nn_pop, nn_reference = NULL, n_species = NULL){

  genome_length <- get_gp()$links %>% nrow

  if(is.null(nn_reference))
    nn_refs <- n_species
  else{
    nn_refs <- nn_reference %>% map('links') %>% map('weights') %>% do.call(rbind, .)
    zeros <- matrix(0, nrow = nrow(nn_refs), ncol = genome_length - ncol(nn_refs))
    nn_refs <- cbind(nn_refs, zeros)
  }

  res <- nn_pop %>% map('links') %>% map((function(x){ y <- rep(0, genome_length); y[x$number] <- x$weights; y})) %>% do.call(rbind, .) %>% kmeans(nn_refs)

  nn_refs <- replicate(nrow(res$centers), get_gp(), simplify = FALSE)
  for(i in 1:length(nn_refs))
    nn_refs[[i]]$links$weights <- res$centers[i,]
  class(nn_refs) <- 'nnpop'

  species <- res$cluster

  list(nn_reference = nn_refs, species = species)
}
