## Set of functions to cache time consuming inverse matrix functions.
## makeCacheMatrix creates an object to store a matrix and cache
## its inverse. cacheSolve takes the object created by makeCacheMatrix
## and returns the inverse. The inverse is cached and returned from cache
## in subsequent calls

## Creates an object too store the matrix passed in and
## returns a list of 4 functions to set and get the matrix
## and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Takes an object returned from makeCacheMatrix and
## returns the inverse matrix. Calculates the inverse
## if the cacheMatrix does not contain a cached value
## Returns the cached value if available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
