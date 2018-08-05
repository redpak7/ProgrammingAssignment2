## functions create a super matrix that can store its content and also its inverse

## Function creates a matrix defined by user defined data x and its column and row parameters
## also stores the inverse of the matrix in the inv variable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## function obtains inverse of matrix x; also checks if determinant of x is 0 and if it is it just returns x rather than its inverse

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$getinverse(inv)
    inv
}
