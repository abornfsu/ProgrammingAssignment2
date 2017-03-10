## makeCachgeMatrix is a function that takes a matrix as an argument and 
## solves for and caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}

get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    }

## cacheSolve is a function that solves for the inverse of a matrix, but... 
## It first checks to see if this inverse matrix has already been found and cached. 
## If not, it solves for the inverse and prints it. 

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
