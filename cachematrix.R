##           Description of cachematrix.R

## Function: makeCacheMatrix
## Arguments: a square matrix to be evaluated.
## Returns: list of functions

## Function: cacheSolve
## Arguments: a matrix  
## Returns: the inverse of square matrix from calculus or cached data

## How to use cachematrix.R
## Load the file        ## source("cachematrix.R")
## Create a matrix      ## mymatrix <- matrix(c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10), ncol = 4, nrow=4)
## Store the matrix     ## mCM <- makeCacheMatrix(mCM)
## Solve the inverse    ## cacheSolve(mCM)


makeCacheMatrix <- function(x = matrix()) {
    # Create a matrix wrapper, that allows for caching the matrix inversion.
    #
    # Arg:
    #   x: The matrix to be evaluated.
    #
    # Return:
    #   A list containing functions to set and
    #   get the matrix and inverse matrix values.
    
    cache.inverse <- NULL
    # Reset the cached value when setting new values to the matrix
    set <- function(y) {
        x <<- y
        cache.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cache.inverse <<- inverse
    getinverse <- function() cache.inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Solves the inverse of the square matrix set with the function makeCacheMatrix.
    # Check if the inverse has already been calculated. 
    # If so, returns the inverse of the matrix from the cache.
    # Otherwise calculate the inverse of the new matrix and sets the
    # new value in the cache.
    #
    # Arg:
    #   x: The square matrix, created with makeCacheMatrix.
    #
    # Returns:
    #   The inverse of the given matrix if it is a square matrix.
    
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("Using cached data")
    } else {
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        message("Setting a new data in cache")
    }
    inverse
}
