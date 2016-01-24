## programming assignment 2 _ 24/01/2016

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    set <- function(y) {
      x <<- y
      matInv <<- NULL
    }
    get <- function() x
    setInverse <- function(invVal) matInv <<- invVal
    getInverse <- function() matInv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matInv <- x$getInverse()
        if(!is.null(matInv)) {
          message("getting cached data")
          return(matInv)
        }
        data <- x$get()
        matInv <- solve(data, ...)
        x$setInverse(matInv)
        matInv
}
