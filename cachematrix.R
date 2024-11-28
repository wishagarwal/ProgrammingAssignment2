## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
    inve <- NULL  # Variable to store the cached inverse
    set <- function(y) {
        x <<- y
        inve <<- NULL  # Reset cached inverse when matrix is changed
    }
    get <- function() x  # Return the matrix
    setInverse <- function(inverse) inve <<- inverse  # Cache the inverse
    getInverse <- function() inve  # Return the cached inverse
    
    # Return a list of functions for interacting with the matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if the inverse is already cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  # Return the cached inverse
    }
    mat <- x$get()  # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse
}
