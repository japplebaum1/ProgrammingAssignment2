## These functions work together to only calculate the inverse of
## a matrix when it has not been calculated before, or if the
## matrix has changed since the last time the inverse was found.

## makeCacheMatrix returns a set of four functions that support
## a self-caching inverse of the matrix.  The functions are:
## get()          Returns the current matrix
## set(m)         Sets the matrix to m
## getinverse()   Returns the current matrix, or NULL if it has
##                not yet been calculated
## setinverse(i)  Stores the inverse i for future retrieval

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix passed in.  If
## it has been calculated before, return the cached value,
## otherwise calculate it now and store it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
