## This script contains 2 functions that supporst matrix inversion 
## caching feature. First function `makeCacheMatrix` creates a special 
## "matrix" object that can cache its inverse. Second function 
## `cacheSolve` retrieve the inverse value from cache if it is
## already calculated else if calculates & updates the cache.

## makeCacheMatrix function creates a special matrix object
## that can cache its inverse. It contains list of functions
## for get the matrix value, set the matrix value, get the 
## inverse matrix value from cache, set the inverse matrix 
## value in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Set Function
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        # Get Function
        get <- function() x
        # Set Inverse matrix value in cache function
        setinverse <- function(inverse) m <<- inverse
        # Get Function for Inverse matrix value in cache
        getinverse <- function() m
        # List of defined functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function returns the inverse matrix value.
## If inverse value exists in cache it returns from cache 
## else it computes the inverse value, update the cache
## and returns the computed value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        # Get the matrix value
        data <- x$get()
        # Compute the inverse of matrix
        m <- solve(data, ...)
        # update the cache
        x$setinverse(m)
        # return the inverse of matrix
        m
}