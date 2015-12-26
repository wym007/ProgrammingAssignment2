## Matrix inversi on is usually a costly computation 
## and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly
## two functions that cache the inverse of a matrix

## makeCacheMatrix: this function creates a special "matrix" object 
## that can cache its inverse

## cacheSolve: this function computes the inversoe of 
## the special "matrix" retured by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrive the inverse from the cache.


## makecacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the stored inverse value to NULL
    cachedInverse <- NULL

    # set value of the matrix
    set <- function(y) {
      x <<- y
      cachedInverse <<- NULL # matrix has changed, reassign to NULL 
    }

    # get value of matrix
    get <- function() x

    # set inverse of matrix
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # get inverse of matrix
    getInverse <- function() cachedInverse

    # return a list containing all function defined above
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculeted 
## (and the matrix has not changed), then
## the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {

    # get inverse
    invFunc <- x$getInverse()

    # if inverse exists, check if already cached
    # if yes, return cached inverse
    if(!is.null(invFunc)) {
      message("getting cached data")
      return(invFunc)
    }

    # if not, get matrix
    data <- x$get()

    # compute inverse of matrix   
    invFunc <- solve(data, ...)

    # cache inverse of matrix
    x$setInverse(invFunc)

    # return inverse
    invFunc
}
