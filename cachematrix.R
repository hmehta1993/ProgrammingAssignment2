## The functions below allow for the computation of inverting a matrix whilst storing
## the input matrix and caching the inverted solution. 

## MakeCacheMatrix creates an object which will store the input matrix
## and cache its inverse (until this function is re-run).

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
      x <<- y
      invm <<- NULL
    }
  get <- function() x
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve function will compute the inverse of the cached input matrix 
## stored by MakeCacheMatrix. If the matrix has already been calculated and
## MakeCacheMatrix has not been re-run, it will retrieve it from the cache (getinverse) and
## display a message

cacheSolve <- function(x, ...) {
  invm <- x$getInverse()
  
  if (!is.null(invm)) {
      message("getting result from cache")
      return(invm)
  }
  matrix <- x$get()
  invm <- solve(matrix, ...)
  x$setInverse(invm)
  invm
}


