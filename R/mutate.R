mutate <- function(nn, em_rate, wm_rate, lm_rate, nm_rate, wm_power, wm_limit){

  nn <- mutate_enables(nn, em_rate)
  nn <- mutate_weights(nn, wm_rate, wm_power, wm_limit)
  if(stats::runif(1) < nm_rate)
    nn <- mutate_node(nn)
  if(stats::runif(1) < lm_rate)
    nn <- mutate_link(nn)

  nn
}

mutate_enables <- function(nn, em_rate){
  disabled <- !nn$links$enabled
  nn$links$enabled[disabled] <- ifelse(stats::runif(sum(disabled)) < em_rate, TRUE, FALSE)

  nn
}

mutate_weights <- function(nn, rate, power, limit){

  n <- nrow(nn$links)
  w <- stats::runif(n, -power, +power)
  w <- ifelse(stats::runif(n) < rate, w, 0)
  w <- nn$links$weights + w
  w <- ifelse(w < -limit, -limit, w)
  w <- ifelse(w > +limit, +limit, w)

  nn$links$weights <- w

  nn
}

mutate_node <- function(nn){  ##innovation number should be given considering the number of new nodes (all first nodes on the same level should have same innovation number, seconds too.. so on)

  nnoriginal <- nn
  genome <- get_gp()

  selected_link <- sample(nrow(nn$links), 1)
  split_link <- nn$links[selected_link, ]
  nn$links[selected_link, 'enabled'] <- FALSE

  nd_genome <- genome$nodes$number[
    genome$nodes$number %in% genome$links$output[genome$links$input == split_link$input] &
    genome$nodes$number %in% genome$links$input[genome$links$output == split_link$output] &
    genome$nodes$layer %in% mean(genome$nodes$layer[genome$nodes$number %in% split_link[c('input','output')]])
    ]

  nd_nn <- nn$nodes$number[
    nn$nodes$number %in% nn$links$output[nn$links$input == split_link$input] &
    nn$nodes$number %in% nn$links$input[nn$links$output == split_link$output] &
    nn$nodes$layer %in% mean(nn$nodes$layer[nn$nodes$number %in% split_link[c('input','output')]])
    ]
  update <- FALSE

  if(length(nd_genome) == 0 | length(nd_genome) == length(nd_nn)){
    number = max(genome$nodes$number) + 1
    update <- TRUE
  }
  else if(length(nd_nn) == 0)
    number = nd_genome[1]
  else
    number = nd_genome[length(nd_nn) + 1]

  if(number %in% nn$nodes$number){  ## talvez trocar a criacao de um novo nó pela conexão do nó existente?
    number = max(genome$nodes$number) + 1
    update <- TRUE
  }

  if(number %in% nn$nodes$number)
    new_node <- NULL
  else
    new_node <- list(
      number = number,
      type = 'hidden',
      layer = mean(genome$nodes$layer[genome$nodes$number %in% split_link[c('input','output')]])
      )
  if(update)
    genome$nodes <- rbind(genome$nodes, new_node)
  nn$nodes <- rbind(nn$nodes, new_node)

  input_node <- c(split_link$input, number)
  output_node <- c(number, split_link$output)
  number <- c(0,0)
  update <- c(FALSE, FALSE)

  if(any(i1 <- (genome$links$input == input_node[1] & genome$links$output == output_node[1])))
    number[1] <- genome$links$number[which(i1)]
  else{
    number[1] <- max(genome$links$number) + 1
    update[1] <- TRUE
  }
  if(any(i2 <- (genome$links$input == input_node[2] & genome$links$output == output_node[2])))
    number[2] <- genome$links$number[which(i2)]
  else{
    number[2] <- max(genome$links$number) + 2
    update[2] <- TRUE
  }

  if(sum(update) == 1)
    stop('check this xit')

  new_links <- list(
    number = number,
    input = input_node,
    output = output_node,
    weights = c(0,0),
    enabled = c(TRUE,TRUE)
  )

  if(all(update))
    genome$links <- rbind(genome$links, new_links)


  new_links$weights <- c(1, split_link$weights)
  nn$links <- rbind(nn$links, new_links)

  storage.mode(nn$nodes$number) <- storage.mode(genome$nodes$number) <- 'integer'
  storage.mode(nn$links$input) <- storage.mode(genome$links$input) <- 'integer'
  storage.mode(nn$links$output) <- storage.mode(genome$links$output)  <- 'integer'

  gene_pool$nodes <- genome$nodes
  gene_pool$links <- genome$links

#  if(length(nn$nodes[,1]) != length(unique(nn$nodes[,1]))){
#    save(list = ls(), file = 'C:\\Dropbox\\temp\\deb.R')
#    stop('repeated node values')
#  }

  nn
}

mutate_link <- function(nn){

  genome <- get_gp()

  inp_line <- -sample(-nn$nodes$number[nn$nodes$type != 'output'], 1)
  input_node <- nn$nodes[nn$nodes$number == inp_line,]

  out_line <- -sample(-nn$nodes$number[nn$nodes$layer > input_node$layer], 1)
  output_node <- nn$nodes[nn$nodes$number == out_line,]

  if(any(l <- nn$link$input == input_node$number & nn$links$output == output_node$number)){
    return(nn)
  }
  if(!any(l <- genome$links$input == input_node$number & genome$links$output == output_node$number)){
    genome$links <- rbind(genome$links, list(
      number = max(genome$links$number) + 1,
      input = input_node$number,
      output = output_node$number,
      weights = 0,
      enabled = TRUE
    ))
    l <- max(genome$links$number)
    gene_pool$links <- genome$links
  }

  new_link <- genome$links[l, ]
  nn$links <- rbind(nn$links, new_link)

#  if(nrow(nn$links[,-1]) != nrow(unique(nn$links[,-1]))){
#    save(list = ls(), file = 'C:\\Dropbox\\temp\\deb.R')
#    stop('repeated link values')
#  }

  nn
}


