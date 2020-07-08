## Put comments here that give an overall description of what your
## functions do

## Wraps a matrix in a function to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newinv) inv <<- newinv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Calculates the matrix inverse if needed

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        message("Inverse not cached, calculating")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    } else {
        message("Found inverse in cache")
    }
    inv
}
