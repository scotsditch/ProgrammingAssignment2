## R Programming Assignment 2


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <-NULL
  
  set <- function(y){
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x = matrix(), ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}


##Example Output:
## > x<-matrix(1:4,2,2)
## > m = makeCacheMatrix(x)
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
