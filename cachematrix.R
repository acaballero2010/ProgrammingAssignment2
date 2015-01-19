## Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## We need to right a pair of functions that cache the inverse of a matrix

## makeCacheMatrix(), this function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                      m <- NULL
                      set <- function(y) {
                        x <<- y
                        m <<- NULL
                      }
                      get <- function() x
                      # I use ginv() from the MASS package, I can't get it work using solve()
                      # It's more dynamic as giv() can take even the non-invertible matrix
                      setInverseMatrix <- function(ginv) m <<- ginv
                      getInverseMatrix <- function() m
                      getevn<- function() environment()
                      list(set = set, get = get,
                           setInverseMatrix = setInverseMatrix,
                           getInverseMatrix = getInverseMatrix,
                           getevn = getevn)
}

## cacheSolve(),this function takes the output of makeCacheMatrix and store it
## this way if similar matrix is supplied it will just retrieve what is in the cache 
cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setInverseMatrix(m)
  m
}

## Loading the MASS package to enable ginv()
library(MASS)

## Testing the functions
## Feeding invertible matrix
x <- matrix(rnorm(100500), 5, 5)
vec <- makeCacheMatrix(x)
cacheSolve(vec)
# Same matrix, run 2x
cacheSolve(vec) # "getting chached data" msg appears 

## Feeding non-invertible matrix
x <- matrix(rnorm(100500), 5, 15)
vec <- makeCacheMatrix(x)
cacheSolve(vec)
## Same matrix, run 2x
cacheSolve(vec) # "getting chached data" msg appears 


