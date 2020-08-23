## R Programming: Assignment 2
##
## These functions allow the caller to cache the result of
## solve() function (compute the inverse) of a matrix. 
## If the cache is empty (getsolve), the result of the cacheSolve function
## is cached (setsolve). If the cache is not empty, the value of the cache 
## returned.


#
## This function takes a matrix argumnet and returns a list with the matrix and
## supporting functions for caching the matrix. The resulting cache matrix object is
## to be used by cacheSolve().
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix, clear the cache
  set <- function(y) {
    x <<- y
    ## reset the cache
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Update the cached matrix value or inverse
  setsolve <- function(inverse) {
    print( "setting cached data")
    inv <<- inverse
  }
  
  # Get the cached matrix value
  getsolve <- function() inv
  
  # List of getters and setters
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#
## This function calls the solve() function to calculate
## the inverse of the matrix created using 
## makeCacheMatrix(), The inverse value is cached. The next time 
## cacheSolve is called, the inverse is not recalculated but
## the cached value is returned.
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()

    if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}
