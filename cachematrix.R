## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to get the data matrix,
## to store the inverted matrix in cache and to return the cached 
## inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(IM) m <<- IM
  getmatrix <- function() m
  list(set = set, get = get,
       setinverted = setinverted,
       getmatrix = getmatrix)
}


## This function receives makeCacheMatrix object and returns the cached inverted
## matrix in case it had been already solved. Otherwise, it retrieves the data
## matrix stored in the makeCacheMatrix object, and return its inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverted(m)
  m
}
