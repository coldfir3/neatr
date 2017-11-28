#' NeuralEvolution of Augmenting Topologies
#'
#'
#' @param fn Function with a single neural network as input and a numeric single fitness value as output
#' @param starting_pop List of neural networks
#' @param max_generations Numerical value for the hard maximum number of generations
#' @param n_stagnate Numerical indicating the number of generations witouth any improvement to cause an early stop
#' @param print_level Numerical value of the printing level: 0 = none, 1 = minimal, 2 = full
#' @param em_rate,wm_rate,lm_rate,nm_rate Numerical in [0,1] of the probability of mutations
#' \itemize{
#'  \item em_rate = enable mutation
#'  \item wm_rate = node mutation
#'  \item lm_rate = link mutation
#'  \item nm_rate = node mutation
#' }
#' @param wm_power Numeric indicating the magnitude of the weight mutation. The perturbation will be sampled
#'  from an unifor distribution [-wm_power, wm_power]
#' @param wm_limit Numeric cap for the weight of a mutated network
#' @param threshold Numerical trheshold of divergence of species
#' @param c1,c2,c3 Numerical weighting constants for the species filter:
#' \itemize{
#'  \item c1 = weight of excess genes
#'  \item c2 = weight of disjoint genes
#'  \item c3 = weight of average weight differences
#' }
#' @param solution_tolerance Numerical value for solution tolerance, two numbers differing this much
#'  will be considered equal
#' @param kill_percentage Numerical [0,1] indicating the fraction of individuals that will be excluded
#'  from the mating proccess.
#' @param number_for_kill Numerical value for minimal population size for killing
#' @param selection.pressure Numerical value used as exponent when evaluating the shared fitness,
#'  higher numbers will make more elite populations reproduce more often
#' @param return_history Logical indicating if the whole history of elite individuals should be returned
#'  (default: FALSE)
#'
#' TODO:
#'   melhorar as mutacoes, ver codigo matlab, mas a ideia e garantir que a mutacao ocorra caso seja sorteada
#'   rever codigo de crossover
#'   melhorar output da funcao neat
#'   criar funcao de pos processamento
#'
#'  ORIGINAL ARTICLE PARAMETERS (IMPLEMENT)
#'
#'  OK - Weight Mutation Range: The maximum magnitude of a mutation that changes the weight of a connection (Section 3.1).
#'  OK - c1: Coefficient representing how important excess genes are in measuring compatibility (equation 3.1).
#'  OK - c2: Coefficient for disjoint genes (equation 3.1).
#'  OK - c3: Coefficient for average weight difference (equation 3.1).
#'  OK - Ct: Compatibility threshold (equation 3.1); when dynamic thresholding is used, this variable determines the starting threshold.
#'  Survival Threshold: Percentage of each species allowed to reproduce (Section 3.3).
#'  Mutate Only Probability: Probability that a reproduction will only result from mutation and not crossover.
#'  Add Node Probability: Probability a new node gene will be added to the genome (Section 3.1).
#'  Add Link Probability: Probability a new connection will be added (Section 3.1).
#'  Interspecies Mating Rate: Percentage of crossovers allowed to occur between parents of different species (Section 3.3).
#'  Mate By Choosing Probability: Probability that genes will be chosen one at a time from either parent during crossover (Section 3.2).
#'  Mate By Averaging Probability: Probability that matching genes will be averaged during crossover (Section 3.2).
#'  Mate Only Probability: Probability an offspring will be created only through crossover without mutation.
#'  Recurrent Connection Probability: Probability a new connection will be recurrent.
#'  Population Size: Number of networks in the population.
#'  Maximum Stagnation: Maximum number of generations a species is allowed to stay the same fitness before it is removed. In competitive coevolution, the worst species is removed if it has been around this many generations (Section 3.3).
#'  Target Number of Species: Desired number of species in the population; used only in dynamic compatibility thresholding (Section 3.3).
#'
#'
#' @export
#' @examples
#' fn <- function(nn){
#'  xor_inputs = rbind(c(0,0),
#'                     c(1,0),
#'                     c(0,1),
#'                     c(1,1))
#'  xor_outputs = c(0,1,1,0)
#'  nn_outputs <- apply(xor_inputs, 1, activate, nn = nn, act_fun = function(x) sigmoid(x, 4.9))
#'  (4 - sum(abs(xor_outputs - nn_outputs)))^2
#' }
#' npop = 30
#' max_generations = 20
#' starting_pop <- populate.nn(npop, 2, 1)
#' optimal_pop <- neat(fn, starting_pop, max_generations, return_history = TRUE)
#'
#'
#' \dontrun{
#'  # the following example should converge to the optima
#'  npop = 150
#'  max_generations = 100
#'  starting_pop <- populate.nn(npop, 2, 1)
#'  optimal_pop <- neat(fn, starting_pop, max_generations)
#' }
#'
#' \dontrun{ # debug routine
#' r <- unclass(lsf.str(envir = asNamespace("neatr"), all = T))
#' for(name in r) eval(parse(text=paste0(name, '<-neatr:::', name)))
#' }
neat <- function(fn, starting_pop,
                 max_generations = 100, solution_tolerance = 0.001, n_stagnate = 15, print_level = 1,
                 kill_percentage = 0.2, number_for_kill = 5, selection.pressure = 2,
                 em_rate = 0.25, wm_rate = 0.9, lm_rate = 0.4, nm_rate = 0.04, wm_power = 2.5, wm_limit = 10,
                 threshold = 6, c1 = 2, c2 = 2, c3 = 1,
                 return_history = FALSE){


  # initializing the genome and history based on initial population
  init_gp(starting_pop[[1]])
  species_history <- NULL
  nn_pop <- starting_pop

  for(gen in 1:max_generations){

    ### Speciate nn_pop and update species_history
    #spec <- speciate(nn_pop, species_history[[gen-1]]$reference, threshold, c1, c2 , c3)
    spec <- speciate_kmeans(nn_pop, species_history[[gen-1]]$reference, 10)
    species <- spec$species
    nn_pop <- species %>% unique %>% sort %>% map(function(x){
      res <- nn_pop[species == x]
      class(res) <- 'nnpop'
      res})
    fitness <- nn_pop %>% map(function(x) map_dbl(x, fn))
    species_history <- c(species_history, list(NULL))
    species_history[[gen]]$reference <- spec$nn_reference
    species_history[[gen]]$size <- table(spec$species)
    species_history[[gen]]$fitness <- fitness %>% map(summary) %>% do.call(rbind,.) %>% as.data.frame

    if(gen < n_stagnate)
      species_history[[gen]]$propagating <- rep(TRUE, length(species_history[[gen]]$size))
    else{
      fitness_history <- suppressWarnings(species_history %>% rev %>% `[`(1:n_stagnate) %>% map('fitness') %>% map('Max.') %>% transpose %>% map(unlist))
      improved <- fitness_history %>% map(unique) %>% map_dbl(length) %>% map_lgl(`>`, 1)
      is_old <- fitness_history %>% map_dbl(length) %>% map_lgl(`==`, n_stagnate)
      species_history[[gen]]$propagating <- improved | !is_old
    }

    ### passing unmutated the best individual of each species (elitism)
    elite_pop <- list(nn_pop, fitness %>% map(order, decreasing = TRUE)) %>% pmap(function(x,y) x[[y[1]]])
    class(elite_pop) <- 'nnpop'
    species_history[[gen]]$elite <- elite_pop

    ### breading new individuals based on their fitness
    ## allocating the number of offsprings for each species
    probability_offspting <- species_history[[gen]]$fitness$Mean^selection.pressure * species_history[[gen]]$propagating
    if(all(probability_offspting == 0))
      stop('all species went extinc')
    n_offspring <- sample(1:length(nn_pop), length(starting_pop) - length(elite_pop), replace = TRUE, prob = probability_offspting) %>% factor(levels = 1:length(nn_pop)) %>% table %>% as.list

    parents <- list(nn_pop, n_offspring, fitness) %>% pmap(get_parent,  number_for_kill, kill_percentage)
    offspring_pop <- parents %>% map(function(x) pmap(x, function(a, b) crossover(a, b, em_rate, wm_rate, lm_rate, nm_rate, wm_power, wm_limit))) %>% unlist(recursive = FALSE) ## check crossover function
    class(offspring_pop) <- 'nnpop'

    nn_pop <- c(elite_pop, offspring_pop)
    nn_pop <- nn_pop[sample.int(length(nn_pop))]
    class(nn_pop) <- 'nnpop'

    cat('generation', gen, 'maximum fitness:', max(species_history[[gen]]$fitness$Max.), '\toffsprings:', unlist(n_offspring) + 1, '\n')
  }

#  list(nn_pop = nn_pop, fitness = fitness, species_history = species_history) # save elite fitness too?

  elite_fitness <-
    species_history %>%
    map('fitness') %>%
    map('Max.') %>%
    map(function(x){length(x) <- length(elite_pop); x}) %>%
    (function(x){x <- do.call(rbind, x) %>% as.data.frame(); names(x) <- paste0('S', 1:length(elite_pop)); x})
  elite_individuals <-
    species_history %>%
    map('elite')
  population_size <-
    species_history %>%
    map('size') %>%
    map(function(x){length(x) <- length(elite_pop); x}) %>%
    (function(x){x <- do.call(rbind, x) %>% as.data.frame(); names(x) <- paste0('S', 1:length(elite_pop)); x})

  if(return_history)
    list(individuals = elite_individuals, size = population_size, fitness = elite_fitness)
  else
    list(individuals = elite_individuals[[gen]], size = population_size[gen,], fitness = elite_fitness[gen,])

}


if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
