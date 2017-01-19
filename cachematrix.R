## The functions makeCacheMatrix and cacheSolve have the purpose
## of caching the inverse of a matrix. 
## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Set the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # cacheSolve function takes this as an input argument
  return(list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse))
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated and the matrix
## has not changed, then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Determines if the inverse has already been calculated
  if(!is.null(inv)) {
    message("Getting cached data!")
    return(inv)
  }
  
  # If the inverse has not yet been calculated, calculate the inverse here
  data <- x$get()
  inv <- solve(data, ...)
  
  # Caching the inverse
  x$setInverse(inv)
  
  return(inv)
}
