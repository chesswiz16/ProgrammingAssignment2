## Programming Assignment 2 for R Programming

## Memoized matrix with get and set functions for caching it's inverse 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Get/compute the inverse of x
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("using cached result")
  } else {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  return(inverse)
}
