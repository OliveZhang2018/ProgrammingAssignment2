## Put comments here that give an overall description of what your
## functions do

## the first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing functions to set and get values of the matrix,
## and set/get the inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)

}


## the following function calculates the inverse of matrix created above. 
## it first checks if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
