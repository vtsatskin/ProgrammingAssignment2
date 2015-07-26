# makeCacheMatrix and cacheSolve saves time on solving the inverse of a matrix
# by caching results to reduce time for subsequent computations.

# Creates a cache matrix object with a list of the following functions
# * get: get the value of the matrix
# * set: set the value of the matrix
# * setmean: sets the value of the inverse of the matrix
# * getmean: gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # Adapted from makeVector code in assignment description
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Takes a cache matrix constructed by `makeCacheMatrix` and computes its inverse.
# The inverse calculated will be cached to reduce repeating the same calculation.
# e.g usage: cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
cacheSolve <- function(x, ...) {
  # Adapted from cachemean code in assignment description
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
