## The following functions will cache the inverse of a matrix rather than compute it repeatedly.
## The <<- operator is used used to preserve the values of the matrix and the inverse. 

##---------------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse.
##---------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  invm <<- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(inverse_matrix) invm <<- inverse_matrix
  getinvm <- function() invm
  list(set = set, get = get, setinvm = setinvm, getinvm = getinvm)
}

##----------------------------------------------------------------------------------------------------
## This function computes the inverse (using solve function in R) of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache and return it.
##----------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data)
  x$setinvm(invm)
  ## Return a matrix that is the inverse of 'x'
  invm  
}
