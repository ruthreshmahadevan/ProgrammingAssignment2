

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # setting i as null which will be a inverse
  smat <- function(n=matrix()) {  #defining smat function
    x <<- n   # value of matrix in original place
    i <<- NULL # if it is new matrix then set the value of inverse as null
  }
  gmat <- function() x # will return the argument of matrix
  sinverse <- function(inv) i <<- inv # assigns value of inverse 
  ginverse <- function() i # gets the value of inverse
  list(smat = smat, gmat = gmat, sinverse = sinverse, ginverse=ginverse)
}

## cacheSolve function 

## This will help to give cached value if not available then it will calculate

cacheSolve <- function(x, ...) {
  i <- x$ginverse() 
  if(!is.null(i)) {
    message("Cached value available and return the cached value")
    return(i)
  }
  data <- x$gmat()
  message("Not available in cache and it will be calculated")
  i <- solve(data, ...)
  x$sinverse(i)
  message("Inverse Matrix")
  i
}
