## Two functions to create a "matrix" & to calculate and cache its inverse.

## Input: an invertible matrix
## Ouput: a list of functions to:
##  1. Change the value stored from input
##  2. Return the value stored from input 
##  3. Set the value of the stored inverted matrix
##  4. Return the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  
  if(!is.matrix(x) | nrow(x) != ncol(x)) error("X must be a square matrix.")
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() x
  
  set.inverted.matrix <- function(mat) inverted.matrix <<- mat
  
  get.inverted.matrix <- function() inverted.matrix
  
  list(set = set, get = get,
       set.inverted.matrix = set.inverted.matrix,
       get.inverted.matrix = get.inverted.matrix)
}


## Returns a matrix that is the inverse of a matrix stored in 'x' 
## where 'x' is the the result of a call to makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inverted.matrix <- x$get.inverted.matrix()
  
  if(!is.null(inverted.matrix)) {
    message("Getting cached data...")
    return(inverted.matrix)
  }
  
  mat <- x$get()
  inverted.matrix <- solve(mat)
  x$set.inverted.matrix(inverted.matrix)
  inverted.matrix
}
