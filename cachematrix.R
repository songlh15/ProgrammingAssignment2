# This pair of functions are to compute and cache the inverse of a matrix,

## This function is to create a matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  mat <- NULL ### #initialize mat
  set <- function(y) { 
  x <<- y  ###assign y to x and make x available in parent environment
  mat <<- NULL ###put NULL to mat and make mat available in parent environment
  }
  
  get <- function() x ###capture input matrix x
  setinverse <- function() mat <<- solve(x) ###set the solved inverse matrix to cache
  getinverse <- function() mat  ### capture solved inverse matrix from cache
  
  list(set = set, get = get,  ### give names to object for easy retrieve 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is to calculate the reverse of the matrix returned by 
## function makeCacheMatrix, if it was previously calculated then retrieve 
## it from the cache 

cacheSolve <- function(x, ...) {
  
  x$setinverse()  ###check the solved inverse of matrix from cache
  invm <- x$getinverse()  ###retrieve inverse of matrix and assign it to invm
  if(!is.null(invm)) {   ###if an inverse exists in cache then 
    message("getting an inverse of matrix!")  ### print out message 
    invm  ### and return it
  return(invm)  ### Return this matrix that is the inverse of 'x'
  }    
  
  matx <- x$get() ###Assign input matrix to matx
  invmx <- solve(matx) ###calculate inverse matrix for matx and assign to invmx
  x$setinverse(invmx) ###put calculated inverse of matrix to cache
  invmx             ###output invmx
}
