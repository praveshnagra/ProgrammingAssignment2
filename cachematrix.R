## The function, makeCacheMatrix creates a special "matrix",
## which contains a list of functions to
## set the matrix
## get the matrix
## set the inverse
## get the invrese

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function calculates inverse in case not already calculated,
## and if calculated, returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  #m <- getinverse(x)
  #m <- x(getinverse())
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #data <- get(x)
  #data <- x(get())
  m <- solve(data, ...)
  x$setinverse(m)
  #setinverse(m)
  #x(setinverse(m))
  m
}
