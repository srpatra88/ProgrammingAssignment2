## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmatrix <- function(matr) {
    x <<- matr
    inv <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(invr) inv <<- invr
  getinverse <- function() inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached reversed matrix")
    return(inv)
  }
  matrx <- x$getmatrix()
  inv <- solve(matrx, ...)
  x$setinverse(inv)
  inv
}
