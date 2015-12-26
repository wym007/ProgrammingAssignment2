## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##
## Create a a special matrix object that can cache its inverse
## so that it calculate the inverse of matrix once 
## but can be used repeatedly to solve the inverse of the matrix  



## Creat a makecacheMatrix object for an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
      x <<- y
      cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## Return the inverse of a makecacheMatrix object


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invFunc <- x$getInverse()
    if(!is.null(invFunc)) {
      message("getting cached data")
      return(invFunc)
    }
    data <- x$get()
    invFunc <- solve(data, ...)
    x$setInverse(invFunc)
    invFunc
}
