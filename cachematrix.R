## Assignment: Caching the Inverse of a Matrix
## This file includes a pair of fuctions that solves the inverse of a matrix and 
## caches the inverse for future use, by making use of scoping rules.
## This code is inspired by the sample cacheVectorMean code provided by the Coursera R Programming Course Team.


## makeCacheMatrix creates a special kind of matrix that can cache the 
## inverse of the matrix once solved.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) i <<- inverseMatrix
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes the special kind of matrix that is created by the makeCacheMatrix function as input.
## Returns its inverse either from its cache or 
## first solves the inverse, caches it and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
