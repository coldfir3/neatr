gene_pool <- new.env()

#' Functon returns the gene pool
#' @export
get_gp <- function(){
  gp <- list(
    nodes = gene_pool$nodes,
    links = gene_pool$links
    )
  class(gp) <- 'nn'
  gp
}

init_gp <- function(nn){
  gene_pool$links <- nn$links
  gene_pool$links$weights <- 0
  gene_pool$links$enabled <- TRUE
  gene_pool$nodes <- nn$nodes
}
