##### Programming Assignment 2 - Lexical Scoping #####
# Write a pair of functions that cache the inverse of a matrix

# 1. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { #set the value
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the value
  setInverse <- function(solveMatrix) inv <<- solveMatrix # set value of inverse matrix
  getInverse <- function() inv # get the value of inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# 2. The following function calculates the inverse of the special
# "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) { # Check if result has already been cached
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) # Compute inverse of square matrix
  x$setInverse(inv)
  inv
}
