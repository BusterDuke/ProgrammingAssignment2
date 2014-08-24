## R File:     cachematrix.R
##
## Author:     Buster Duke
##
## Date:       8/23/14
##
## Overview:   These functions allow a user to store and access a matrix
##             and its inverse using get and set functions.  When calculating
##             the inverse of a matrix, the matrix object is checked to 
##             determine if the inverse is already computed.  If so, it will
##             return the 'cached' value; otherwise, it will compute and store 
##             the matrix inverse and cache the value for subsequent requests.
## 

##             makeCacheMatrix()
## makeCacheMatrix creates an object that stores a matrix which can be
## passed in as an argument to the function or stored through the set()
## function.  The matrix inverse is stored in inv and is set through
## the setinv() function.  The matrix and its inverse can be retrieved
## through the get() and getinv() functions respecively.
##
## Finally, if makeCacheMatrix is invoked without using either set, get,
## setinv or getinv, the list() function is invoked to display the
## current values of each of the aforementioned functions.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL ## Initialize to null until calculated...
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

## cacheSolve()
## cacheSolve() takes a special matrix created using the makeCacheMatrix()
## function and returns its inverse as calculated by the R solve() function.
## cacheSolve first checks to see if the matrix inverse has already been
## calculated.  If so, the cached value is returned.  If not, this funciton
## gets the matrix and computes its inverse.

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