## These functions create an object that can store matrix and
## its inverse. When calculating an inverse, a cached copy will
## be used if presented.

## makeCacheMatrix creates an object that stores a matrix (x)
##  and its inverse (m). There are set and get functions to 
##  set and get both the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinv = setinverse,
       getinv = getinverse)
}


## cacheSolve receives an object created by makeCacheMatrix
##  and return the inverse of matrix. If there is a cached copy
##  it is returned immediately. If there is no cached copy, the
##  function calculates inverse, set it in the object, and
##  return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
