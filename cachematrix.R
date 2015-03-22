## Overall Description
## The functions creates a special matrix that can store in the cache the inverse of the matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse by:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## computes the inverse of a special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, cachesolve retrieves the inverse 
## from the cache. Otherwise it calculates the inverse of a matrix and stores it 
## in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# Test Run
matrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
cMatrix <- makeCacheMatrix(matrix)
cacheSolve(cMatrix)
cacheSolve(cMatrix)