## These two functions will find the inverse of a square matrix (when one exists) and save
## the inverse in a special object that can be recalled at a later time instead of 
## calculating the inverse again.

## This function creates an empty matrix. Then it solves for the inverse of a matrix that
## the user enters as an argument and saves the inverse to the empty matrix.

makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function takes the first function as an argument, it checks to see if the empty 
## matrix created by the first function has been filled. If it has been filled, this 
## function returns that matrix. If the empty matrix created by the first function has 
## not been filled, it calls up the matrix entered as the argument to the first function, 
## solves for the inverse and saves the inverse in the empty matrix created by the first
## function.

cachesolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}