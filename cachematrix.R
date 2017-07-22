## These two functions cache and retrieve the inverse of a matrix 
## if it is available,rather than recalculating it, to save computation time.

## This function creates a list of four functions to cache and retrieve a matrix and an inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL        
  }
  
  get <- function() x
  setinverse <- function(xinverse) x_inv <<- xinverse
  getinverse <- function() x_inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function retrieves the cached inverted matrix if it is availale, and if not available 
## it calculates and stores the inverted matrix so it can be retrieved in future.

cacheSolve <- function(x, ...) {
  x_inv <- x$getinverse()       
  if(!is.null(x_inv)) {      
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()       
  x_inv <- solve(data)
  x$setinverse(x_inv)         
  x_inv
}
