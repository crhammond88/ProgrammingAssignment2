## Create special matrix objects that can cache their inverse to prevent unnecessarily 
## repeating computations

## Creates a special matrix object that can cache the inverse of itself
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## Caches or retrieves the inverse of a special matrix (x) and returns it
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() 
  if(!is.null(inverse)) {
    message("Getting cached inverse matrix")
    return(inverse)
  }
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}