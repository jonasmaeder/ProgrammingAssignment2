## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("value not yet cached")
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

c <- makeCacheMatrix(matrix(c(1, 2, 2, 2), nrow=2, ncol=2) )
cacheSolve(c)
cacheSolve(c)
