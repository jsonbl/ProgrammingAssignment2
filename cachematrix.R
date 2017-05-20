## jsonbl
## Coursera: Programing in R
## Week 3 Programming Assignment 2
## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## The function takes a single value that defines the dimensions of a square matrix filled with random values.
## Optional second argument of TRUE/FALSE can be passed to decide if the inverse matrix is cached (default = TRUE) or not
## usage example: myMatrix <- makeCacheMatrix(5, cache=TRUE)
## output is a list that stores a vector, its matrix and its inverse matrix (if cache=TRUE)
## ex: myMatrix$getv() returns the vector, myMatrix$getm() returns the matrix, and 
##     myMatrix$getinv() returns the invers matrix

makeCacheMatrix <- function(x = matrix(), cache=TRUE) {
  v <- sample(x*x)
  m <- matrix(v,nrow=x,ncol=x)
  getv <- function() v
  getm <- function() m
  if(cache == TRUE){
    getinv <- function(x){
      solve(m)
    }
  }
  else{
    getinv <- function() inv <- NULL
  }
list(getv = getv, getm = getm, getinv = getinv)
}

## This function retrieves the inverse matrix from the cache of the first matrix if available. 
## If null, then the inverse matrix is calculated.

cacheSolve <- function(x) {
  inv <- x$getinv()
  if(!is.null(inv)){
    inv <- x$getinv()
    message("getting cached data...")
    return(inv)
  }  
  else{
    message("cached data is not available, calculating...")
    m <- x$getm()
    inv <- solve(m)
    return(inv)
  }
}
