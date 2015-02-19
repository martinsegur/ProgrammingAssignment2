## Martin Ségur-Cabanac, February 2015
## Defines a specail matrix object with functions for setting and getting
## the matrix itself, plus setting and getting the inverse of the matrix
## Assumes the matrix is a square, inversible matrix
## No error handling included!

## Creates the special matrix ojbect
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) x <<- y
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## Looks for an existing cached inverse matrix, creates it and stores it
## in the special matrix object if none found
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message ("getting cached inverse of matrix")
        return(inv)
    }
    data <- x$get()
    ## Return a matrix that is the inverse of 'x'
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Below commented line can be used to create a sample matrix for testing
## test <- matrix( rnorm(10*10,mean=0,sd=1), 10, 10)