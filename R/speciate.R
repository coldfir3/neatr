speciate <- function(nn_pop, nn_reference = NULL, threshold, c1, c2, c3){

  if(is.null(nn_reference))
    nn_refs <- nn_pop[1]
  else
    nn_refs <- nn_reference

  species <- NULL

  for(i in 1:length(nn_pop)){
    for(j in 1:length(nn_refs)){
      if(delta(nn_pop[[i]], nn_refs[[j]], c1, c2, c3) < threshold){
        species <- c(species, j)
        break
      }
    }
    if(length(species) < i){
      nn_refs <- c(nn_refs, nn_pop[i])
      species <- c(species, j+1)
    }
  }

  class(nn_refs) <- 'nnpop'

  list(nn_reference = nn_refs, species = species)
}

delta <- function(nn, nn_ref, c1, c2, c3){

  gene_pool <- list(nn, nn_ref) %>% map('links') %>% map('number') %>% unlist %>% unique %>% sort

  N <- length(gene_pool)

  genome <- gene_pool %in% nn$links$number
  genome_ref <- gene_pool %in% nn_ref$links$number

  genome_diff <- !(genome & genome_ref)
  genome_diff_rle <- rle(genome_diff)

  if(rev(genome_diff_rle$values)[1])
    E <- rev(genome_diff_rle$lengths)[1]
  else
    E <- 0

  D <- sum(genome_diff_rle$lengths[genome_diff_rle$values]) - E



  W <- mean(abs(nn$links$weights[nn$links$number %in% gene_pool[which(!genome_diff)]] -
             nn_ref$links$weights[nn_ref$links$number %in% gene_pool[which(!genome_diff)]]))

  (c1 * E + c2 * D)/N + c3 * W
}

speciate_weights <- function(nn_pop, species_history = NULL, threshold, c3){

  nn_pop <- nn_pop

  if(is.null(species_history))
    nn_refs <- nn_pop[1]
  else
    nn_refs <- species_history$reference

  species <- NULL

  for(i in 1:length(nn_pop)){
    for(j in 1:length(nn_refs)){
      if(delta_net_weights(nn_pop[[i]], nn_refs[[j]], c3) < threshold){
        species <- c(species, j)
        break
      }
    }
    if(length(species) < i){
      nn_refs <- c(nn_refs, nn_pop[i])
      species <- c(species, j+1)
    }
  }

  class(nn_refs) <- 'nnpop'

  list(nn_reference = nn_refs, species = species)
}

delta_net_weights <- function(nn, nn_ref, c3){
  w <- nn$links$weights
  w_ref <- nn_ref$links$weights
  mean(abs(w - w_ref)) * c3
}

