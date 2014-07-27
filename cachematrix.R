## To use caching functionality with matrices, use makeCacheMatrix to 
## make a matrix with this feature and use cacheSolve to calculate its inverse
## whenever needed.

## makeCacheMatrix creates special 'matrix' that's actually a list containing 
## functions to set or get the matrix and set or get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
        set <- function(some.matrix) {
                x <<- some.matrix
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse really fast if it has been calculated once (by caching)
## otherwise calculates, caches and returns the inverse. It expects a special matrix
## made using makeCacheMatrix as its first argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
