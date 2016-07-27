## Matrix inversion is usually a costly computation and 
## There may be some benefit to caching the inverse of a matrix 
## The below 2 functions acheive the caching of inverse of a matrix

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ##'inv' is a matrix which is the inverse of x 
  ## its cached value
  inv <- NULL
  
  ## set the value of matrix with new one
  set <- function(y) {
      x <<- y
      ## since its a new matrix, the inverse value doesnt hold true
      ## hence initializing it back to NULL
      inv <<- NULL
  }
  
  ## function to return the current matrix 
  get <- function() x	
  
  ## cache the inverse of current matrix 
  setinverse <- function (invMtrx) inv <<- invMtrx
  
  ##return the already cached inverse of the matrix
  getinverse <- function() inv
  
  ##return value is a list of all these functions
  list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by  
## makeCacheMatrix  above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##get the cached matrix-inverse
  invMatrix <- x$getinverse()
  
  ##check to see if its already calculated or Not yet calculated (NULL)
  if (!is.null(invMatrix)) {
    message("Getting the cached value")
    return (invMatrix)
  }
  else {
    ## get the current matrix
    mtrx <- x$get()
    ## compute the inverse
    invMatrix <- solve(mtrx,...)
    
    ##cache the computed value
    x$setinverse(invMatrix)
    
    ##return the cacluated value
    invMatrix
  }	
}
