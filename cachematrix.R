## The pair of functions below do the following:
## A) makeCacheMatrix: crates a special "matrix" object that can cache it's inverse
## B) cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix.

## makeCacheMatrix description: This function creates a cache of the inverse of a matrix 
## through the following steps 
# 1) It sets the value of the matrix 
# 2) It gets the value of the matrix
# 3) It sets the value of the inverse of the matrix (the solve function calculates the inverse of a matrix)
# 4) It gets the value of the inverse of the matrix

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


## cacheSolve description: This function calculates the inverse of the matrix created 
## with the function above through the following steps.
# 1) It checks to see if the inverse has already been calculated
# 2) If so, it gets the inverse from the cache and skips the computation
# Otherwise, it calculates the inverse of the data, sets the value of 
# the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
        ## Returns a matrix that is the inverse of 'x'
}
