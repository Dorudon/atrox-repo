## Caching the Inverse of a Matrix

## Create a matrix

makeCacheMatrix <- function(x = matrix()) {
    a <- NULL
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) a <<- solve
    get_inv <- function() a
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## This function computes the inverse of the matrix 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    a <- x$get_inv()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data, ...)
    x$set_inv(a)
    a
}
