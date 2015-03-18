## The functions makeCacheMatrix and cacheSolve below are used to create a
## special object that stores a matrix and caches its inverse.
##
## These functions are my solution to Programming Assignment 2 in the course
## "R Programming" at Johns Hopkins University.

## Example on how to use the special "matrix" object is found below. Note how
## the inverse is retrieved from the cache the second time. 
#
# > myMatrix <- matrix(1:4, 2, 2)
# > x <- makeCacheMatrix(myMatrix)
# > cacheSolve(x)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(x)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse 
## from the cache, (and print a little message about it).

cacheSolve <- function(x, ...) { 
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
