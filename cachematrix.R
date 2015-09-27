## These functions are to be called on a matrix in order to cache the value of
## the inverse of the matrix.

## Creates a special object with a list of functions to set/get the matrix
## as well as set/get the inverse of the matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes the result of makeCacheMatrix as an argument and returns the inverse.
## If the inverse has already been cached it will be returned without recalculating.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
