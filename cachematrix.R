## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a "special" matrix with
## get and set functions to get and set the matrix and 
## get and set the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

 # Initializing the inverse of the matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL # Sets the inverse to null if the matrix changed
        }
        get <- function() x
        
        # Getter setter methods for inverse
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
## This function takes in a "special" matrix and 
## checks if the inverse of the matrix has already been
## computed. If yes, then it returns the cached inverse.
## Else, it computes the inverse, sets the inverse and 
## returns the same

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        # Returns the cached inverse if the inverse existed
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Computes the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
