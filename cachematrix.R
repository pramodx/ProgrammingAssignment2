## Creating two functions - 1 is like a constructor function that set and gets a matrix and then
## sets and gets the inverse of the matrix

## The makeCacheMatrix takes in a square matrix and have the setter getter functions to set the matrix
## and also set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { inv <<- inverse }
  getinverse <- function() { inv }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  
}

## The cacheSolve function takes the matrix as an argument and checks whether it has a cached inverse
## If not the inverse is generated and cached.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
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
