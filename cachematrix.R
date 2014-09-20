## R Programming - Assignment 2

## makeCacheMatrix: Take a square invertible matrix and return a list 
## containing functions 
##  1. set the value of the matrix 
##  2. get the value of the matrix 
##  3. set the value of the inverse 
##  4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set = function(y) {
    # Operator "<<-" to assign a value to an object in an environment 
    # that is different from the current environment
    x <<- y
    inverseMatrix <<- NULL
  }
  get = function() x
  setInverse = function(inverse) inverseMatrix <<- inverse
  getInverse = function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: Take the matrix of makeCacheMatrix() and compute its inverse

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  # If the inverse has already been calculated
  if (!is.null(inverseMatrix)){
    # Retrieve the inverse from the cache
    message("getting cached data")
    return(inverseMatrix)
  } else {
    # Else calculate the inverse with the solve function
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
  }
}