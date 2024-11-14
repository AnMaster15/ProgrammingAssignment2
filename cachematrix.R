## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse. It contains methods to set and get
## the matrix and its inverse.
makeCacheMatrix <- function(m = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  ## Method to set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL  # Reset inverse cache if matrix changes
  }
  ## Method to get the matrix
  get <- function() {
    m  # Return the matrix
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    i  # Return the inverse property
  }
  ## Return a list of the methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The cacheSolve function computes the inverse of the special matrix
## created by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Get the cached inverse if available
  m <- x$getInverse()
  
  ## Return the cached inverse if it exists
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Retrieve the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse of the matrix
  m <- solve(data, ...)
  
  ## Cache the calculated inverse
  x$setInverse(m)
  
  ## Return the inverse matrix
  m
}
