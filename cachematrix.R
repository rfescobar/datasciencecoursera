## Create an object to store a matrix and cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  ref <- NULL
  set <- function(y){
    x << -y 
    ref <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)ref << -inverse
  getInverse <- function()ref
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix 
## returned by the above function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ref <- x$getInverse()
  if(!is.null(ref)){
    message("getting cached data")
    return(ref)
  }
  mat <- x$get()
  ref <- solve(mat, ...)
  x$setInverse(ref)
  ref
}