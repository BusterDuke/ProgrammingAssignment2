## R File:     cachematrix.R
##
## Author:     Buster Duke
##
## Date:       8/23/14
##
## Overview:   These functions allow a user to store and access a matrix
##             and its inverse using get and set functions.  When calculating
##             the inverse of a matrix, the matrix object is checked to 
##             determine if the inverse has already computed.  If so, it will
##             use the 'cached' value; otherwise, it will compute the inverse
##             and cache the value for future requests.
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}