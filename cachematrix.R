## These functions are modified from the second programming assignment for the R Programming Coursera course
## taught by Roger D. Peng, Jeff Leek, and Brian Caffo.

## The function 'makeCacheMatrix' creates a list of four functions, set, get, setsolve, and getsolve.
## The function 'set' stores a matrix, and should avoid being called outside 'makeCacheMatrix.'
## The function 'get' allows the user to retrieve a matrix stored by 'makeCacheMatrix'.
## The function 'setsolve' allows the user to store the inverse of the matrix stored by 'set', and should not be called outside 'makeCacheMatrix.'
## The function 'getsolve' allows the user to retrieved the stored inverse of the matrix.

## The function 'makeCacheMatrix' takes a single argument, a matrix x to cache. The inverse
## of x may be computed and cached using 'cacheSolve'.
## Intended use: u <- makeCacheMatrix(x) caches 'x'.
##               cacheSolve(u)
##               u$get() retrieves 'x'.
##               u$getsolve() retrieves the matrix inverse of 'x', if it has been set by calling cacheSolve(u)
##
## If cacheSolve(u) has not been called, the u$getsolve() returns NULL.



makeCacheMatrix <- function(x = matrix()) {
  ## 'm' holds the inverse of the matrix x, initialize to NULL.
  m <- NULL
  
  ## create functions 'set' and 'get' used for setting and retrieving the cached matrix 'x'.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  ## create functions 'setsolve' and 'getsolve' used for setting and retrieving the cached inverse of the matrix 'x'.
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  ## return an object consisting of functions caching the matrix 'x', and allowing the caching of the inverse of 'x' via 'cacheSolve'.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



cacheSolve <- function(u, ...) {
  ## Return the inverse of 'u$get()$, where 'u' is an object created by 'makeCacheMatrix'.
  ## If this is the first time calling 'cacheSolve' on 'u', the inverse is cached in 'u' so that it may be
  ## retrieved by 'u$getsolve()'.
  ## Subsequent calls to 'u$getsolve()' will retrieve the cached inverse, so that the inverse does not need to be computed again.
  
  ## check to see if the inverse is already cached, and, if so, return it
  m <- u$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## because the inverse is not already cached, compute, cache, and then return it
  data <- u$get()
  m <- solve(data, ...)
  u$setsolve(m)
  m
}
