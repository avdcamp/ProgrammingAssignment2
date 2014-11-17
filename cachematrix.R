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
  setinv <- function(inverse) { m <<- inverse }
  getinv <- function() { m }
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
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
