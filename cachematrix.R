## This file contains two separate functions which are designed to 
## help speed up matrix computations by caching the inverse of a matrix.

## Completed as part of R Programming course in Coursera Data Science track.
## Work by @jcbrodie. 21-05-2015. 


## This function creates and returns a special matrix object that can cache its inverse. 
## It contains functions for getting and setting the values of the input
## matrix and its inverse. Used by cacheSolve(). 
makeCacheMatrix <- function(x = matrix()) {
    
    #We assume that the input is actually a matrix, so don't have to check
    
    #First initialize matrix inverse to NULL since it's currently unknown
    inv.matrix <- NULL
    
    #Function to (re-)set the matrix and inverse values
    set <- function(y) {
        x <<- y
        inv.matrix <<- NULL
    }
    
    #Function to get the input matrix values
    get <- function() x
    
    #Functions for getting and setting the cached inv.matrix (inverse of the 
    #input matrix) values
    setInverse <- function(solve) inv.matrix <<- solve
    getInverse <- function() inv.matrix
    
    #Store the values in a list for easy access
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes and returns the inverse of the special matrix (input x) 
## that is returned by makeCacheMatrix(). 
## 
## If the inverse has already been calculated then this function retrieves
## the inverse from the cache.
cacheSolve <- function(x, ...) {

    inv.matrix <- x$getInverse()
    
    if(!is.null(inv.matrix)) {
        message("getting inverse from cache")
        return(inv.matrix)
    }
    
    #here we invert the matrix in the case that it didn't already exist
    mat <- x$get()
    inv.matrix <- solve(mat, ...)
    x$setInverse(inv.matrix)
    #return the inverse
    inv.matrix
}