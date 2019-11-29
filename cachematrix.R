## Put comments here that give an overall description of what your
## functions do


## This function  should creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n
  list(set = set, get = get,
       getInverse = getInverse,
       setInverse = setInverse)

}


## This function computes the inverse of "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated.
## Next, it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getInverse()
  if(!is.null(n)){
          message("getting cached data")
          return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setInverse(n)
  n
}
