## Creates a special "matrix" object that can cache its inverse.
## Input matrix should be square and invertible
makeCacheMatrix <- function(x = matrix()) {
  mat <- x 
  set <- function(y) {
    x <<- y
    mat <<- x
  }
  get <- function() x
  setsolve <- function(solve) mat <<- solve
  getsolve <- function() mat
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getsolve() ## inversed matrix
  mat2 <- x$get()   ## input matrix
    
  if(!all(mat == mat2))  {
    message("getting cached data")
    return(mat)
  }
  
  mat <- solve(mat2)
  x$setsolve(mat)
  mat
}
