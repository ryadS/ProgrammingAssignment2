## The makeCacheMatrix and the cacheSolve functions allows the inverse of a matrix that is calculated
## once to be stored in the cache and reused for future inverse calculations of the same matrix.

## function that takes a matrix as an argument and contains a function to set the value of the matrix
## and another function to get the value of the matrix. The makeCacheMatrix has two other functions
## that set and get the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
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


## The cacheSolve function take a matrix, x, and returns the inverse of the matrix x. This function
## first get the value of the inverse of x and checks if the inverse has alreay been calculated. If
## it has a value, gets the inverse from the cache and return it. If the inverse is not calculated
## it calculates the inverse and sets the cache by calling the setinverse function.

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
