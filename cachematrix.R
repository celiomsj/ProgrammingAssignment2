##
# Coursera
# R Programming
# Programming Assignment 2: Lexical Scoping
#
# The objetive of this R code is to create a function that caches the value
# of the inverse of a matrix.
# 
# Example usage is as follows:
# > myMatrix <- matrix(rnorm(9), 3, 3)
# > myMatrixCache <- makeCacheMatrix(myMatrix)
# > cacheSolve(myMatrixCache)
# > cacheSolve(myMatrixCache)
#
# The second cacheSolve call uses the cached value.
#


# makeCacheMatrix returns a special object that stores a matrix and a
# cached measure

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cache) m <<- cache
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
  
}


# cacheSolve checks if there is a cached value for x, a object created by
# makeCacheMatrix. If it doesn't exist, computes x inverse, caches it and
# return its value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}
