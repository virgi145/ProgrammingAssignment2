## This function has two funtions, the first one is 'makeCacheMatrix'
## that creates a special matrix object that can cache its inverse.
## The second function 'cacheSolve' computes the inverse of the 
## special matrix returned by 'makeCacheMatrix'. If the inverse
## has already been calculated (and the matrix has not changed),
## then the 'cacheSolve' should retrieve the inverse from the
## cache.

cachematrix <- function() {

## Stores a list of functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse,
## and get the value of the inverse.

  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
  }


## Computes the inverse of the matrix or retrieve it from cache

  cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }

list(makeCacheMatrix=makeCacheMatrix, cacheSolve=cacheSolve)
  
}