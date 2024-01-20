## Put comments here that give an overall description of what your
## functions do

## The matrix-to-inverse computation is made more efficient by using a cache.
## Once the inverse matrix is computed, it is stored in the cache 
## and the cache is returned when it is computed again.

## Write a short comment describing this function

## List of functions to place matrices and inverses in cache
## It contains
## a function that sets matrix in the cache
## a function that returns matrix
## a function that sets the inverse matrix in the cache
## a function that returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) inv <<- inverse
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}


## Write a short comment describing this function

## First, check if the inverse matrix has already been computed.
## If so, retrieve the inverse matrix from the cache and skip the computation. 
## Otherwise, compute the inverse matrix and set its value. 
## setinv function sets the inverse matrix to the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
