## These functions are used to apply lexical scoping to a matrix and its inverse operation

## Thus function creates an object of a square matrix and a place for its inverse matix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse= getInverse)
}


## this function checks for a cached inverse for a square matrix.
##    if the inverse is not found, then it calculates the inverse of the square matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
