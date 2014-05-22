# This Function is to calculate reverse of a matrix and put into parent environment
# when needed it can be recalled from cache instead of doing calculation again


## This function initialize matrix x, put it into cache mat and 
## save calculated matrix to cache 


makeCacheMatrix <- function(x = matrix()) {
 
  mat <- NULL ### #initialize mat
  set <- function(y) { 
    x <<- y  ###assign y to x and make x available in parent environment
    mat <<- NULL ###put NULL to mat and make mat available in parent environment
  }
  
  get <- function() x ###capture input matrix x
  setinverse <- function() mat <<- solve(x) ###set the solved inverse matrix to cache
  getinverse <- function() mat  ### capture solved inverse matrix from cache
  
  list(set = set, get = get,  ### make a list for easy retrieve 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function find if a inverse matrix of x is already calculated, 
## if yes then return the inverse matrix, if not then calculate the 
## inverse matrix of x and return it

cacheSolve <- function(x, ...) {
  
  x$setinverse()  ###check the solved inverse matrix from cache
  invm <- x$getinverse()  ###get inverse matrix and assign it to invm
  if(!is.null(invm)) {   ###if an inverse matrix exists then do following
    message("getting an inverse matrix!")  ###print out message 
    invm ### print this inverse matrix
  return(invm)  ### Return this matrix that is the inverse of 'x'
  }    
  
  matx <- x$get() ###Assign cache matrix to matx
  invmx <- solve(matx) ###calculate inverse matrix for matx and assign to invmx
  x$setinverse(invmx) ###put calculated matrix to cache
  invmx             ###output invmx
}
