## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. These
## functions cache the inverse of a matrix.

## makeCacheMatrix creates a list with functions to set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix, and get the inverse
## of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix. First it checks if the inverse 
## has already been computed. If so, it gets and displays the result. It computes
## the inverse and sets the value in the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
