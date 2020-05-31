## This cachematrix.R consist of two function makeCacheMatrix() and cacheSolve()
## Together they create a invese of a matrix, which is created by first function'
## The inverse matrix is cached upto the next matrix is input.


## This fuction creates a matrix object. Which is then used to get the inverse matrix
## This function takes a square invertible matrix as input eg:matrix(1:4,2,2)
## matrix(c(2,4,-3,-7),2,2)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function take the matrix object created by makeCacheMatrix().
## Then find the inverse value whether it is present or not.
## If the inverse value is present then the cached inverse value is assigned
## If not then the inverse of the matrix object is executed and transfer to the former.


cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
