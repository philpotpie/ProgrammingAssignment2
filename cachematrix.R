## ProgrammingAssignment2 of Coursera R Programming course

## function makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## ## Parameter: matrix object
## ## Return: matrix object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## function cacheSolve: computes inverse of the special "matrix" returned by makeCacheMatrix.
## ## if inverse has already been calculated and matrix has not changed, retrieve the inverse from cache.
## ## Parameter: matrix object
## ## Return: inverse matrix object (may be cached retrieval)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
