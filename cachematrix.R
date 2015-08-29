## Caching the Inverse of a Matrix
## Ex: 
## A <- matrix (c(1,2,3,4), 2,2)
## B <- makeCacheMatrix(A)
## cacheSolve(B)  ## run ones gives calculated result (inverse of matrix A)
## cacheSolve(B)  ## run more then ones gives cached result (inverse of matrix A)

## The A matrix should be square (2x2, 3x3, ...)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  ## lists these 4 functions as parameter fields for makeCacheMatrix() 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

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
