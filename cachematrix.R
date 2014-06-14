## A pair of functions that cache the inverse of a matrix.
##
## Recommended reference: 
## http://www.statmethods.net/advstats/matrix.html
## 
## Author: Mark Peng (markpeng73@gmail.com)

# This function creates a special "matrix" object 
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize a variable to cache the inverse of current matrix
  invX <- NULL
  
  set <- function(newX) {
    x <<- newX
    # reset cached inverse matrix of x
    invX <<- NULL
  }
  get <- function() x
  setinv <- function(solvedInv) invX <<- solvedInv
  getinv <- function() invX
  
  # return a list of pointers for defined functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix defined above. 
# If the inverse has already been calculated,
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # try to get the inverse of current matrix
  invX <- x$getinv()
  
  if(!is.null(invX)) {
    message("Getting cached inverse data ...")
    return(invX)
  }
  
  # if not found, compute a new one for it
  origX <- x$get()
  invX <- solve(origX, ...)
  x$setinv(invX)
  
  ## Return a matrix that is the inverse of 'x'
  invX
}

##################################################################
# Normal way to compute the inverse of a square invertible matrix:
# set.seed(1)
# X <- matrix(rnorm(9), nrow = 3, ncol = 3)
# invX <- solve(X)
#
# [Test case]
# Create a new cache matrix m:
# set.seed(1)
# X <- matrix(rnorm(9), nrow = 3, ncol = 3)
# m <- makeCacheMatrix(X)
# 
# Check the content of its original matrix:
# m$get()
#
# The value of inverse matrix should be NULL:
# m$getinv()
#
# Compute the inverse for the first time:
# cacheSolve(m)
# 
# Now the value of inverse matrix should appear 
# as the output of previous line:
# m$getinv()
#
# Check if we can inverse back to the original
# (shoud be the same as m$get()):
# solve(m$getinv())
# 
# Then, call cachesolve(x, ...) again to see if cache works
# (should see the message 'Getting cached inverse data ...'):
# cacheSolve(m)
#
# All done!

