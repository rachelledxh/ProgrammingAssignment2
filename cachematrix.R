## These two functions are used to Caching the Inverse of a given invertible Matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## The first function, makeCacheMatrix creates a special "matrix", 
  ## which is really a list containing a function to
 ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverse of the matrix
  ## get the value of the inverse of the matrix
  
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inve <<- solve
  getinverse <- function() inve
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inve <- x$getinverse()
  ## if the inverse has already been calculated
  if(!is.null(inve)) {
    ## get it from the cache and skip the computation
    message("getting cached data")
    return(inve)
  }
  ## otherwise calculate the inverse
  data <- x$get()
  inve <- solve(data, ...)
  ## sets the value of the inverse of "x" in the cache using the setinverse function 
  x$setinverse(inve)
  return(inve)
}
