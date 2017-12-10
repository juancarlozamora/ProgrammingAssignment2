## The functions are for my Assignmen in Week 3 of Coursera Course Data Science: R Programming 2017
## Week 3 Assignment; Quarter 4 of year 2017; GitHub by user:juancarlozamora

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##argument with default mode of matrix
  inv <- NULL ## initialize inv as NULL; will hold value of matrix inverse 
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

## computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

