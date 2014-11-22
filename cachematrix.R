## Following are a set of two functions makeCacheMatrix() and 
## cacheSolve() that cache the inverse of a matrix instead of 
## calculating its inverse repeatedly.


## This function creates a special matrix object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special matrix calculated
## by the makeCacheMatrix() function above. If the inverse has already
## been calculated then the function should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
