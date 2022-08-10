## Functions for caching the inverse of a matrix

## Create a cache matrix for storing functions required
## Returns: list
##  ["set"] = set matrix
##  ["get"] = get matrix
##  ["setinverse"] = set inverse of matrix
##  ["getinverse"] = get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # remove any existing values of i
  i <- NULL
  
  # assign the variables x and i. The original and inverse matrices 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # return the original value of the matrix
  get <- function() x
  
  # assign the variable i with the value provided
  setinverse <- function(inverse) i <<- inverse
  
  ## return the variable i
  getinverse <- function() i
  
  # return a list containing the cache functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinverse(i)
  i
}
