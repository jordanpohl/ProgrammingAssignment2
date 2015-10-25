## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {

## Make sure matrix is square
 
  if (nrow(x$get) != (ncol(x$get))){
    message("Not possible to invert a rectangular matrix")
    return()
  }
## Check to see if inverse is in cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  }
