## makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
## cacheSolve(): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

## @x: a square invertible matrix
## @return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse matrix
##              4. get the inverse matrix
##         this list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverse, getInverseMatrix = getInverse)
}

## @x: output of makeCacheMatrix()
## @return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return (inverseMatrix)
  }
  matrix <- x$getMatrix()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}