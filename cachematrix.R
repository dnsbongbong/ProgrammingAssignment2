## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
### This function returns an inverse of a set matrix and 
### caches this inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
###This function calculates the inverse of the special "vector" created with
###the makeCacheMatrix function. Notably, it first verifies whether the inverse
### has already been calculated. If this is the case, it returns the inverse
### from the cache and skips the computation. If not, it calculates the inverse
### of the data and sets the value of the inverse in the cache via the 
### setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
