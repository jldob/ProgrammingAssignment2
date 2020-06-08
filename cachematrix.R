## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than computing it repeatedly.
## These functions cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix, actually a list, 
## containing functions to set and get the value of the matrix
## and to set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        x1 <- NULL
        set <- function(y) {
                x <<- y
                x1 <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) x1 <<- inv
        getinverse <- function() x1
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix created
## with the above function if it hasn't already been calculated

cacheSolve <- function(x, ...) {
        x1 <- x$getinverse()
        if(!is.null(x1)) {
                message("getting cached data")
                return(x1)
        }
        data <- x$get()
        x1 <- solve(data, ...)
        x$setinverse(x1)
        x1
}
