# The following codes are written in order to meet the requirements from
# Associate Professor Roger D Peng at BloomBerg School of Public Health at 
# John Hopkins University;
# it is also a requirement from Coursera for completing the short training course.

# According to requirements, I need to creat two functions: makeCacheMatrix
# and cacheSolve. Where:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# and cacheSolve: This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
# not changed), then cacheSolve should retrieve the inverse from the cache.
# makeCacheMatrix creates a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  InvertMatrix <- NULL # this is where the outcome of inversion is collected
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    InvertMatrix <<- NULL # InvertMatrix will be initialized to
  }
  # get the value of the matrix
  get <- function() x
  
  # invert the matrix and store in cache
  setInv <- function(inverse) InvertMatrix <<- inverse
  
  # get the inverted matrix from cache
  getInv <- function() InvertMatrix
  
  # return the created functions to the working environment
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


# The following function returns the inverse of the matrix. 
cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  outcome <- x$getInv()
  
  # if the inverse has already been calculated
  if(!is.null(outcome)) {
    # get it from the cache and skips the computation.
    message("Transfering cached data.")
    
    # display matrix in console
    return(outcome)
  }
  
  # otherwise, calculates the inverse 
  data <- x$get()
  outcome <- solve(data,...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setInv(outcome)
  outcome
}

