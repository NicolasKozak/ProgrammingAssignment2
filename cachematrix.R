## Assignment 2
## Caching the inverse of a matrix

# The function makeCacheMatrix uses the "<<-" operator, 
# together with lexical scoping for efficiency

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(y) inv <<- y
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# The function cacheSolve checks if the matrix inverse
# is already stored and computes it otherwise

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setinv(inv)
    inv
}
