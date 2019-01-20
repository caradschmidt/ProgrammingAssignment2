## The funtion creates a cache of an inverse of the matrix that
## can we retrieved if required in the future.

## This function creates the cache of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set<- function(y) {
        x<<- y
        m <<- NULL
    }
    get <- function() x
        setinv <- function() m<<- solve(x)
    getinv <- function() m
    list (set = set, get = get, setinv= setinv, getinv = getinv)
}


## This function returns cached inverse if it already exists
## or calculates it if it does not exist in cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
            message ("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}