## R Programming Assignment 2 - lexical scoping
## by Xiangheng Ma

makeCacheMatrix <- function(x = matrix()) {
 ## Creates a special "matrix" object that can cache its inverse. 
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() return(x)
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() return(inv)
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting the cached inversed matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  return(inv)
}
