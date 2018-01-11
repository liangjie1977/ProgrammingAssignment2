## Here you will see two funcations for Assignment 2:
## makeCacheMatrix and cacheSolve 


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached inverse")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}
