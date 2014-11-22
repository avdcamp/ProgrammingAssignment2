## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a list with the 'methods' that can
## be used by second function cacheSolve, which will try to get
## a cached result of matrix inversion

## this function creates a list with methods
## set, get, setinv and getinv. These can be 
## used on a matrix object x
makeCacheMatrix <- function(x = matrix(),...) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { x }
  ## store result of inverse in superenvironment
  setinv <- function(inverse) { m <<- inverse }
  ## try to retrieve stored inverse matrix
  getinv <- function() { m }
  ## list of methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The cacheSolve function first checks whether the matrix
## has already had its inverse calculated.
## if so, it returns the cached result
## If not, then it will calculate the inverse
## and store its result in the cache and return the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## First try to get inverse from other environment by callin getinv() function
  m <- x$getinv()
  ## If we find the inverse we can return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Otherwise call get function on x
  data <- x$get()
  ## execute matrix inversion
  m <- solve(data, ...)
  ## Store the result via setinv function
  x$setinv(m)
  ## return value of m
  m
}
