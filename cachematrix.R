## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse cache when a new matrix is set
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the cached inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the cached inverse
    getInverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the matrix created by makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve cached inverse if available
    
    if (!is.null(inv)) {  # If inverse is already cached, return it
        message("Getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)  # Compute inverse using solve()
    x$setInverse(inv)  # Cache the computed inverse
    
    inv  # Return the inverse
}
