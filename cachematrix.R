# This pair of functions are to compute and cache the inverse of a matrix. 

## This function is to create a matrix object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 
  mat <- NULL ### #Initialize mat.
  set <- function(y) { 
  x <<- y  ###Assign y to x and make x available in parent environment.
  mat <<- NULL ###Put NULL to mat and make mat available in parent environment.
  }
  
  get <- function() x ###Capture input matrix x.
  setinverse <- function() mat <<- solve(x) ###Set the solved inverse matrix to cache.
  getinverse <- function() mat  ### Capture solved inverse matrix from cache.
  
  list(set = set, get = get,  ### Give names for easy retrieval.
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is to calculate the reverse of the matrix returned by 
## function makeCacheMatrix, if it was previously calculated then retrieve 
## it from the cache. 

cacheSolve <- function(x, ...) {
  
  x$setinverse()  ###Check the solved inverse of matrix from cache.
  invm <- x$getinverse()  ###Retrieve inverse of matrix and assign it to invm.
  if(!is.null(invm)) {   ###If an inverse exists in cache then 
    message("getting an inverse of matrix!")  ### print out message 
    invm  ### and return it.
  return(invm)  ### Return invm, which is the inverse of 'x'.
  }    
  
  matx <- x$get() ###Assign input matrix to matx.
  invmx <- solve(matx) ###Compute inverse matrix for matx and assign it to invmx.
  x$setinverse(invmx) ###Put computeded inverse of matrix to cache.
  invmx             ###Output invmx.
}
