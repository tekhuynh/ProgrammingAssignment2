## A matrix which can cache its inverse.

## Initialises the matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cached_inverse <- NULL
        set <- function(new_matrix) {
                x <<- new_matrix
                cached_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) cached_inverse <<- inversed
        getinverse <- function() cached_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the given cachable matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
