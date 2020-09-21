## The aim is to write two functions: makeCacheMatrix and cacheSolve
## makeCacheMatrix creates a special "matrix" object that can cache its inverse and cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
