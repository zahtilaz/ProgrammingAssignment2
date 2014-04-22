## Matrix inversion can be a costly operation.
## If the inverse of the matrix is required in several
## locations in the code, it is better to compute the inverse once 
## and cache it, and use the cached inverse later.
## A pair of functions that cache the inverse of a matrix follows.
## Example how to use:
## > zzmat <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3)
## > zzSpecMat <- makeCacheMatrix()
## > zzSpecMat$set(zzmat)
## > cacheSolve(zzSpecMat) 
## > cacheSolve(zzSpecMat) ## returns cached value

## makeCacheMatrix function simply provides the help functions
## to get/set the original matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) inv <<- solved
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function first tries to get the cached 
## inverse, if there is one, using an instance of makeCacheMatrix 
## function. 
## If there is not inverse in the cache, cacheSolve computes the 
## inverse and stores it in the cache.
## Subsequent calls to cacheSolve will return the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

