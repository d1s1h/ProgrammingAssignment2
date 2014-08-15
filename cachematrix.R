## Matrix inversion is usually a costly computation and there is some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## The functions below allow the creation of special "matrix object", which is
## capable of cahcing an R matrix and its inverse. 

## Given an R matrix "x", makeCacheMatrix creates the aligned "matrix object" 
## providing the operations:
## 	set(x) to initialize with a new R matrix
##	get() to retrieve the R matrix
##	setinverse(inv) to store the inverse
##	getinverse() to retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes "matrix object", created by makeCacheMatrix, and returns the inverse
## of the cached R matrix. Tt first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the cached R matrix and caches the inverse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}