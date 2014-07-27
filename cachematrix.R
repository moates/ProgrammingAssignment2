##makeCacheMatrix and cacheSolve are used to compute and cache the
##inverse of a matrix, avoiding re-doing costly computations.

## makeCacheMatrix creates a vector containing a list of functions to
## get & set the values of the given vector for caching

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
     
          x <<- y
          inv <<- NULL
     }
     
     get <-function() { x }
     setInverse <- function(inverse) { inv <<- inverse }
     getInverse <- function() inv
     
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
     }


## cacheSolve takes a cacheMatrix, checks for a cached inverse, else
## computes, caches, and returns the inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
             message("getting cached inverse")
             return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
