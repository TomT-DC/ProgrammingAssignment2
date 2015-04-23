## Only small changes were necessary to convert this R program to inverting a matrix
# rather than computing the mean of a vector

## makeCacheVector is a storage dump.
#  It takes as input 'x' a square invertable matrix.
#  Once makeCacheVector has been run it holds the original matrix as 
# the variable 'x$get', and it holds the inverse of the 
# original martix as 'x$getmean. Initially the inverse x$getmean is set to NULL.

## cacheSolve function has a primary argument the special vector previously created
# by makeCacheVector. 
# cacheSolve() DOES NOT TAKE THE ACTUAL INVERTABLE MATRIX AS ITS INPUT.

#  When cacheSolve is run it outputs an inverted version of  the original
# square matrix that was fed into makeCacheVector.
#  Before performing the inversion, cacheSolve checks with the special vector
#  created by makeCacheMatrix to see if a previous inversion has 
# already been cached. If so, ti takes the inverted matrix from the special storage vector.
# If an inversion has not already been performed, cacheSolve performs an inversion and saves
# the inversion results back in the special storage vector.


# makeCacheVector must be run first with the input argument being a
#square invertable vector.
makeCacheVector <- function(x = numeric()) {
  # m is where the inverted matris is stored. Intitially it is NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get is where the original matrix x is stored
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve() takes as input the storage vector created by makeCacheVector
cacheSolve <- function(x, ...) {
# It checks if the storage vector already has an inverse stored in $getinverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If the inverse was not already there, one is created and then 
  # stored in the storage spot $setinverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

