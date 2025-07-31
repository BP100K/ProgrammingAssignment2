## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a matrix object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve computes the inverse of the matrix that makeCacheMatrix returns
##If the inverse is already stored, it gets it from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)){
    message("getting data from cache")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
