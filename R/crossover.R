crossover <- function(nn_a, nn_b, em_rate, wm_rate, lm_rate, nm_rate, wm_power, wm_limit){  ### REVER

  # get inovation lists
  numbers_a <- sort(nn_a$links$number)
  numbers_b <- sort(nn_b$links$number)
  numbers_missing <- !(numbers_a %in% numbers_b)

  genome_length <- length(numbers_a)

  # get crossover list
  crossover_list <- ifelse(!numbers_missing, sample(c('a','b'), genome_length, replace = TRUE), rep('a', genome_length))

  # do the crossover
  nn <- nn_a
  nn$links <- do.call(rbind, map2(crossover_list, numbers_a, function(parent, i){
    if(parent == 'a')
      nn_a$links[nn_a$links$number == i,]
    else if(parent == 'b')
      nn_b$links[nn_b$links$number == i,]
    else
      stop('Parent must be "a" or "b"')
  }))

  nn <- mutate(nn, em_rate, wm_rate, lm_rate, nm_rate, wm_power, wm_limit)

  return(nn)
}

get_parent <- function(nn_pop, n_offspring, fitness, number_for_kill, kill_percentage){

  rank <- fitness %>% map(order, decreasing = TRUE)

  kill_flag <- nn_pop %>% map(length) %>% map(`>`, number_for_kill)
  kill_number <- nn_pop %>% map(length) %>% map(`*`, kill_percentage) %>% map(round)
  kill_id <- list(kill_flag, kill_number, rank) %>% pmap(function(x, y, z){
    if(x)
      return(rev(z)[(1:y)])
    else
      NA
  })

  if(!any(is.na(kill_id))){
    nn_pop <- nn_pop[-kill_id]
    fitness <- fitness[-kill_id]
  }
  id_a <- sample(length(nn_pop), n_offspring, replace = TRUE) ## change to stochastic BS ##https://en.wikipedia.org/wiki/Stochastic_universal_sampling
  id_b <- sample(length(nn_pop), n_offspring, replace = TRUE) ## change to stochastic BS
  fittest <- list(id_a, id_b) %>% pmap_chr(function(a, b) ifelse(fitness[a] > fitness[b], 'a', 'b'))
  parents_a <- nn_pop[list(id_a, id_b, fittest) %>% pmap_dbl(function(a, b, f) ifelse(f == 'a', a, b))]
  parents_b <- nn_pop[list(id_a, id_b, fittest) %>% pmap_dbl(function(a, b, f) ifelse(f == 'b', a, b))]
  class(parents_a) <- class(parents_b) <- 'nnpop'

  list(parents_a, parents_b)
}
