## Functions to Cache an Inverse Matrix in the Global Environment  
## And Return the Cached Inverse Matrix or Provide an Inverse Matrix for Input of New Matrix/Matrices

## This function takes the input of a matrix, computes the inverse of the matrix, 
## caches the resulting inverse matrix, and sets functions to retrieve the cached
## matrix in the cacheSolve function

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


## This function takes the input of a matrix and checks to see if the inverse has already been 
## calculated. If it has, it will retrieve the cached matrix. If not, it will calculate the input matrix'
## inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

