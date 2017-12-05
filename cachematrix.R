## The following functions cache the inverse of a matrix.


## makeCacheMatrix function creates a matrix object and contains functions that get/set the matrix and get/set the inverse of that matrix. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set_matrix <- function(matrix){
    x <<- matrix
    inv <<- NULL
  }
  get_matrix <- function() x
  set_inv <- function(inv_mat) inv <<- inv_mat
  get_inv <- function() inv
  list(set_matrix = set_matrix, get_matrix = get_matrix, set_inv = set_inv, get_inv = get_inv)
}


## cacheSolve function solves for the inverse of the given matrix. If the inverse is already computed, then the cached inverse is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}
