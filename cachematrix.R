### These functions cache the inverse of a matrix so it can be looked
### up rather than recomputed.  

### This function creates a list of functions to be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

### This function returns the inverse of a matrix.  First is looks
### to see if it has been cached.  If not, it computes the inverse.  

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data!")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}