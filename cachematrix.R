## This pair of functions takes a matrix and 
## stores its inverse to prevent excessive computing times


## This function creates a special matrix with an 
## inverse value stored. It is interacted with heavily
## in the cacheSolve function. This function also contains
## functions to retrieve the base matrix, as well as 
## set and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function retrieves the inverse of the matrix, 
## or sets and stores the inverse if it does not exist.
## To do so this function calls parts of the previous function
## in order to retrieve the inverse and store its value.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse() 
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
