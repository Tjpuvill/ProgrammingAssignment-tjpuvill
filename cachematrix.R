
## makeCachematrix: makes a list of functions that will enable cachesolve 
## set: a function that can set the matrix or return it to NULL
## get: a function that can retrieve the reversed matrix from memory
## setinverse: a function that can set the reversed matrix 
## getinverse: a function that can retrieve the reversed matrix from memory
## cacheSolve: either returning the reversed matrix from memory or calculating the new one



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve 
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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
