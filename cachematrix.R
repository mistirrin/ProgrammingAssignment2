## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This object contaings methods to get and set a matrix as well as it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
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


## Write a short comment describing this function
## This functions accepts a makeCacheMatrix object,
## it returns the cached inverse if it exists,
## if it doesn't, it coputes the inverse using solve,
## caches the inverse in the input object and returns
## the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
