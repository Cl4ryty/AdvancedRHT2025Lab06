#' Brute force knapsack (non-optimized)
#' Solve the knapsack problem using brute-force search, iterating over all possible combinations.
#'
#' @param x data.frame with two variables v and w describing the values and weights of the objects to fit into the knapsack
#' @param W a numeric scalar that is is a positive, whole number describing knapsack size (maximum weight it can fit)
#' @param parallel a boolean specifying whether the search should be parallelized, defaults to FALSE
#'
#' @returns a list containing value, the combined value of the elements fitted into the knapsack, and elements, the indexes of the elements that are the best fit
#' @export
#'
#' @examples
#' x <- data.frame(v=c(5,4,3,2), w=c(4,3,2,1))
#' brute_force_knapsack_(x=x, W=6)
brute_force_knapsack_ <- function(x, W, parallel=FALSE){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  if(!("v" %in% names(x)) || !("w" %in% names(x))){
    stop("x must contain the variables v and w")
  }
  if(any(x<=0)){
    stop("x must contain only positive values")
  }

  if(W%%1 != 0 || W <= 0){
    stop("W must be a positive whole number")
  }

  # get vector of numbers 1:2^n
  n <- nrow(x)
  numbers <- c(1:2^n)
  # get binary representations
  binary <- intToBits(numbers)
  bit_matrix <- matrix(binary, ncol = length(binary)/length(numbers), byrow=TRUE)
  # get only the columns relevant for us
  bit_matrix <- bit_matrix[,1:n]

  if(parallel){
    values <- unlist(parallel::mclapply(1:nrow(bit_matrix), function(i){
      sum(x$v[bit_matrix[i,]==1])
    }, mc.cores = parallel::detectCores()))
    weights <- unlist(parallel::mclapply(1:nrow(bit_matrix), function(i){
      sum(x$w[bit_matrix[i,]==1])
    }, mc.cores = parallel::detectCores()))
  }else{
    values <- apply(bit_matrix, 1, function(row){
      sum(x$v[row==1])
    })
    weights <- apply(bit_matrix, 1, function(row){
      sum(x$w[row==1])
    })
  }

  # set values with too high weights to negative infinity
  values[weights > W] = -Inf

  # get the row with the highest value
  best_combination_index = which(values==max(values))

  # get the indices of the elements in the combination for this best value
  elements = which(bit_matrix[best_combination_index,]==1)

  # return the value and the elements
  list(value=max(values), elements=elements)

}

#' Brute force knapsack
#' Solve the knapsack problem using brute-force search, iterating over all possible combinations.
#'
#' @param x data.frame with two variables v and w describing the values and weights of the objects to fit into the knapsack
#' @param W a numeric scalar that is is a positive, whole number describing knapsack size (maximum weight it can fit)
#' @param parallel a boolean specifying whether the search should be parallelized, defaults to FALSE
#'
#' @returns a list containing value, the combined value of the elements fitted into the knapsack, and elements, the indexes of the elements that are the best fit
#' @export
#'
#' @examples
#' x <- data.frame(v=c(5,4,3,2), w=c(4,3,2,1))
#' brute_force_knapsack(x=x, W=6)
brute_force_knapsack <- function(x, W, parallel=FALSE){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  if(!("v" %in% names(x)) || !("w" %in% names(x))){
    stop("x must contain the variables v and w")
  }
  if(any(x<=0)){
    stop("x must contain only positive values")
  }

  if(W%%1 != 0 || W <= 0){
    stop("W must be a positive whole number")
  }

  # get vector of numbers 1:2^n
  n <- nrow(x)
  # get binary representations
  binary <- intToBits(c(1:2^n))
  bit_matrix <- matrix(binary, ncol = length(binary)/(2^n), byrow=TRUE)
  # get only the columns relevant for us
  bit_matrix <- bit_matrix[,1:n]

  values <- rep(-Inf, 2^n)
  weights <- rep(Inf, 2^n)
  if(parallel){
    cores <- parallel::detectCores()
    if(.Platform$OS.type == "unix") {
      # Unix/Linux/Mac: Use mclapply (forking - fast)
      values <- unlist(parallel::mclapply(1:nrow(bit_matrix), function(i){
        sum(x$v[bit_matrix[i,]==1])
      }, mc.cores = cores))

      weights <- unlist(parallel::mclapply(1:nrow(bit_matrix), function(i){
        sum(x$w[bit_matrix[i,]==1])
      }, mc.cores = cores))

    } else {
      # Windows: Use parLapply (sockets - slower)
      cl <- parallel::makeCluster(cores)

      # Export necessary objects to cluster
      parallel::clusterExport(cl, c("x", "bit_matrix"), envir=environment())

      values <- unlist(parallel::parLapply(cl, 1:nrow(bit_matrix), function(i){
        sum(x$v[bit_matrix[i,]==1])
      }))

      weights <- unlist(parallel::parLapply(cl, 1:nrow(bit_matrix), function(i){
        sum(x$w[bit_matrix[i,]==1])
      }))

      # Stop cluster
      parallel::stopCluster(cl)
    }
  }else{
    t <- matrix(rep(x$v, 2^n), ncol=n, byrow=TRUE)
    t[bit_matrix!=1] <- 0
    t2 <- matrix(rep(x$w, 2^n), ncol=n, byrow=TRUE)
    t2[bit_matrix!=1] <- 0
    values <- rowSums(t)
    weights <- rowSums(t2)
  }


  # set values with too high weights to negative infinity
  values[weights > W] = -Inf

  # get the row with the highest value
  best_combination_index = which(values==max(values))[1]

  # get the indices of the elements in the combination for this best value
  elements = which(bit_matrix[best_combination_index,]==1)

  # return the value and the elements
  list(value=round(max(values)), elements=elements)


 }


#' Dynamic programming knapsack solver
#' Solve the knapsack problem using the dynamic programming in advance algorithm.
#' This iterates over all possibles values of w to find the maximum value obtainable for the weight.
#'
#' @param x data.frame with two variables v and w describing the values and weights of the objects to fit into the knapsack
#' @param W a numeric scalar that is is a positive, whole number describing knapsack size (maximum weight it can fit)
#'
#' @returns a list containing value, the combined value of the elements fitted into the knapsack, and elements, the indexes of the elements that are the best fit
#' @export
#'
#' @examples
#' x <- data.frame(v=c(5,4,3,2), w=c(4,3,2,1))
#' knapsack_dynamic(x=x, W=6)
knapsack_dynamic <- function(x, W){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  if(!("v" %in% names(x)) || !("w" %in% names(x))){
    stop("x must contain the variables v and w")
  }
  if(any(x<=0)){
    stop("x must contain only positive values")
  }

  if(W%%1 != 0 || W <=0){
    stop("W must be a positive whole number")
  }
  n <- nrow(x)
  m <- matrix(data=0, nrow=n+1, ncol=W+1)
  # browser()
  for(i in 2:(n+1)){
    for(j in 1:(W+1)){
      if (x$w[i-1] > (j-1)){
        m[i, j] <- m[i-1, j]
      }else{
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
    }
  }

  knapsack <- function(i, j){
    # browser()
    if(i == 1){
      return(NULL)
    }
    if(m[i, j] > m[i-1, j]){
      return(c(i-1, knapsack(i-1, j-x$w[i-1])))
    }else{
      return(knapsack(i-1, j))
    }
  }
  # return list of results, reverse elements so that they are in the same order
  # as for the other functions
  return(list(value=round(m[n+1, W+1]), elements=rev(knapsack(n+1, W+1))))

}


#' Greedy knapsack solution approximation
#' Use a greedy heuristic to approximate the solution to the knapsack problem.
#' This sorts objects by their value per weight and greedily picks the top objects until the sack is full.
#'
#' @param x data.frame with two variables v and w describing the values and weights of the objects to fit into the knapsack
#' @param W a numeric scalar that is is a positive, whole number describing knapsack size (maximum weight it can fit)
#'
#' @returns a list containing value, the combined value of the elements fitted into the knapsack, and elements, the indexes of the elements that are the best fit
#' @export
#'
#' @examples
#' x <- data.frame(v=c(5,4,3,2), w=c(4,3,2,1))
#' greedy_knapsack(x=x, W=6)
greedy_knapsack <- function(x, W){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  if(!("v" %in% names(x)) || !("w" %in% names(x))){
    stop("x must contain the variables v and w")
  }
  if(any(is.na(x))){
    stop("x must contain only positive values")
  }

  if(any(x<=0)){
    stop("x must contain only positive values")
  }

  if(W%%1 != 0 || W <=0){
    stop("W must be a positive whole number")
  }
  n <- nrow(x)
  x$i <- c(1:nrow(x))
  x$unit_per_weight <- x$v/x$w
  x <- x[order(x$unit_per_weight, decreasing=TRUE), ]
  x$cumsum <- cumsum(x$w)
  x$cumsum[x$cumsum>W] <- -Inf

  value=sum(x$v[x$cumsum>-Inf])
  elements = sort(x$i[x$cumsum>-Inf])
  return(list(value=round(value), elements=elements))

}
