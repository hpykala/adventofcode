input <- readLines('input08.txt')
input <- as.integer(strsplit(input, ' ')[[1]])

nodeValue <- function(node) {
  if(node$n_child == 0) {
    value <- sum(node$meta)
  } else {
    ref <- node$meta[node$meta <= node$n_child]
    if(length(ref) == 0) {
      value <- 0
    } else {
      value <- sum(sapply(ref, function(i) node$children[[i]]$value))
    }
  }
  return(value)
}

createNode <- function(input) {
  node <- list()
  node$n_child <- input[1]
  node$n_meta <- input[2]
  input <- input[-(1:2)]
  node$children <- list()
  node$sum_meta <- 0
  for(i in seq_len(node$n_child)) {
    node$children[[i]] <- createNode(input)
    input <- node$children[[i]]$rest_input
    node$sum_meta <- node$sum_meta + node$children[[i]]$sum_meta
  }
  node$meta <- input[seq_len(node$n_meta)]
  node$sum_meta <- node$sum_meta + sum(node$meta)
  node$value <- nodeValue(node)
  node$rest_input <- input[-seq_len(node$n_meta)]
  return(node)
}

tree <- createNode(input)

#part 1
tree$sum_meta

#part 2
tree$value