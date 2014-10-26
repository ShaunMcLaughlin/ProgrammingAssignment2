## The functions below can be used to find the inverse of a matrix
## and to store its value so that it can be used again without
## calculating it every time.

## The makeCacheMatrix function takes a matrix as argument and returns
## a list of functions:
##    set         - assigns a new value to the stored matrix (and removes the stored value
##                  of the matrix inverse)
##    get         - returns the value of the stored matrix
##    getinverse  - returns the stored value of the matrix inverse
##    setinverse  - stores a new value of the matrix inverse

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


## cacheSolve takes as argument a list of functions as output by the function
## makeCacheMatrix above
## It then checks if a value for the inverse is stored already.
## If so, then it returns this value
## If not, then it calculated the inverse and stores it for future use, and returns the inverse

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
