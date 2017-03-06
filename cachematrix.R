## Package includes a function to create a cached matrix
## and to calculate its inverse. The cachedSolve function
## assumes the inbound matrix is invertible

## Creates a cachedMatrix 'class' that can hold the matrix
## and it's inverse. This is similar to a 'new' function
## in othe languages

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # getter/setter for the key (inbound matrix)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  # getter/setter for value (inverse of matrix)
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  
  # Return the available functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of the numeric matrix 
## x must be if type cacheMatrix and be initialized
## through the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
  } else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  }
  inv
}
