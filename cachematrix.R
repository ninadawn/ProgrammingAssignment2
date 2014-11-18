## These functions are an alternative to using solve() to 
## compute the inverse of a matrix. They will cache the
## result the first time it needs to be computed and 
## return that result in subsequent calls, saving 
## computation time.

## makeCacheMatrix creates a variant matrix that is 
## capable of caching its inverse. If the matrix is 
## altered, the cached inverse is dumped.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL ## reset inverse to null if matrix is changed
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is used like solve to get the inverse of
## a matrix. However, if the inverse has already been 
## computed, cacheSolve returns the cached result, 
## rather than re-computing.

cacheSolve <- function(x, ...) {
    
    ## Return the cached value if it exists
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## Compute and cache the value if not already done
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse    
}
