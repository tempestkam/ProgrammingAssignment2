## Put comments here that give an overall description of what your
## functions do

## A special matrix, actually a function that creates a list of functions

## Write a short comment describing this function

## The list of functions are
## A function that sets data in the cache
## A function that returns data
## A function that sets the inverse matrix in cache
## A function that returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) inv <<- solve
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}


## Write a short comment describing this function

## First checks if the inverse matrix has already been computed. 
## If so, retrieve the inverse matrix from cache and skip the computation. 
## If not, it calculates the inverse matrix of the data and sets the value 
## of the inverse matrix in the cache with the setinv function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
