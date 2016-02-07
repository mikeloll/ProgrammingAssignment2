## Functions for creating cached matrix inversions, which expire
## when the matrix is modified.

## makeCacheMatrix returns a list of four functions which wrap
## a matrix.  These functions provide getter and setter functions
## ('get' and 'set') to get the raw matrix, as well as getter and
## setter functions to get the inverse of the matrix ('getInverse'
## and 'setInverse').
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  mtrx <- x

  ## set the matrix, also resets the inverse cache
  set <- function(y) {
    mtrx <<- y
    inv <<- NULL
  }

  ## get the matrix
  get <- function() mtrx

  ## set the inverse of the matrix
  setInverse <- function(newInv) inv <<- newInv

  ## gets the matrix's inverse from the cache
  getInverse <- function() inv

  ## the list or "api" for manipulating the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse matrix of the supplied matrix
## (created using the makeCacheMatrix function)
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()

  ## null check to see if this was a cache hit or miss
  if(!is.null(inv)) {
    ## cache hit
    message("getting cached data")
  } else {
    ## cache miss, we need to compute and populate the cache
    message("cache miss, computing matrix inverse")
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
  }

  inv
}
