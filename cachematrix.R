## The makeCacheMatrix() and cacheSolve() functions are an integrated pair of
## functions to resolve a time critical issue in calculating the inverse
## of large matrices. 


## The makeCacheMatrix creates a set of functions to manipulate the matrix,
## and as such are the only methods to access the inverse and the matrix 
## under question.

makeCacheMatrix <- function(x = matrix()) {
    cacheinv <- NULL

    setmatrix <- function(y) {
        x <- y
        cacheinv <- NULL
    }
    getmatrix <- function() { x }
    
    setinverse <- function(inverse) { cacheinv <- inverse }
    getinverse <- function() { cacheinv }
    
}


## The cacheSolve checks if a stored (cached) inverse exists otherwise
## computes the inverse of 'x' and stores this as the new value.
## This calls the predefined 'mat_inverse' function, this is so that 
## updates to the algorithm  stay hidden from general matrix functions.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    cacheinv <- x$getinverse()
    if (!is.null(cacheinv)) {
        message("getting cached data")
        return(cacheinv)
    }
    
    matdata <- x$getmatrix()
    cacheinv <- mat_inverse(matdata)
    x$setinverse(cacheinv)
    
    cacheinv
}
