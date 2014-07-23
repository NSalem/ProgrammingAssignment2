## These functions allow to get the inverse of a matrix and 
## cache it

makeCacheMatrix <- function(x = matrix()) { 
    ## creates a cacheable matrix    
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



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' and caches it
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