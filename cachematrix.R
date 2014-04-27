## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
##   of a matrix rather than computing it repeatedly. The following pair of functions can be used to cache 
##   the inverse of a matrix.

## This function creates a special "matrix" object (a list) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                       # Initialization
  set <- function(y) {                            # set function
    x <<- y
    m <<- NULL
  }
  get <- function() x                             # get function
  setInverse <- function(Inverse) m <<- Inverse   # setInverse function
  getInverse <- function() m                      # getInverse function
  list(set = set, get = get,                      # List of the previously defined functions
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##   retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()               # Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {                 # There is a previously calculated, cached matrix
    message("Getting cached data")
    return(m)
  }
  data <- x$get()                   # There is no cached inverse matrix
  m <- solve(data, ...)             # We have to calculate it
  x$setInverse(m)                   # And then we have to save it to the cache
  m  
}
