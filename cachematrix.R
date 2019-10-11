## Functions to create a special kind of matrix that can cache its inverse to avoid repeated calculations,
## and to calcualte the inverse if necessary

## This function holds information on a given matrix, x, and its inverse inv (if this has been calculated). Can
## set or retrive the values of the inverse and matrix as necessary. Assumes it is only passed invertible matrices.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function pulls an inverse from the CacheMatrix object, or calculates it if it has not previously been calculated.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
}
