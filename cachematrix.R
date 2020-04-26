## makeCacheMatrix creates a special “matrix” that will be used in cacheSolve
## 

## The first function, makeCacheMatrix creates a special “matrix”, 
## which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## when the function ends, it returns a fully formed object of type makeCacheMatrix() 


makeCacheMatrix <- function(x = matrix()) {
  h <- NULL
  set <- function(y) {
    x <<- y
    h <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) h <<- inverse
  getinverse <- function() h
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated then cacheSolve retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  h <- x$getinverse()
  if(!is.null(h)) {
    message("getting cached inverse")
    return(h)
  }
  data <- x$get()
  h <- solve(data, ...)
  x$setinverse(h)
  message("calculating inverse")
  h
}
