##################################################
##           Function makeCacheMatrix           ##
##################################################

# Function makeCacheMatrix 'extends' the matrix x
# by its inverse. The output of makeCacheMatrix 
# contains the matrix x and its inverse, denoted
# 'inv'. If the inverse of 'x' is not known 
# (it has not been calculated yet) the function
# makeCacheMatrxi sets its value to NULL
makeCacheMatrix <- function(x = numeric()) {
  
  inv <- NULL # when a new object of 
  # makeCacheMatrix is constructed, 
  # its inverse is set to NULL 
  # because it is not known.
  
  # function 'set' assigns new value to the 
  # matrix x 
  set <- function(y) {
    x <<- y  # operator '<<-' used here assigns 
    # the value 'y' to the 'x' in the 
    # parent environment, i.e. 'x'in the
    # output of the makeCacheMatrix
    
    inv <<- NULL # if the value of the matrix 'x'
    # has changed, the its inverse
    # must be set to NULL (because)
    # it is not known
  }
  
  # function 'get' returns the value of the matrix 'x'
  get <- function() x
  
  # function 'setinv' is used to set the inverse of 
  # matrix x. Operator '<<-' is used here to assign
  # the value of the inverse to the field 'inv' in 
  # the parent environment
  setinv <- function(inverse) inv <<- inverse
  
  # function 'getinv' returns the inverse of 'x'
  getinv <- function() inv
  
  # The output of 'makeCacheMatrix' is a list of 
  # functions (operators) used to assign and return
  # values of 'x' and its inverse
  list(
    set = set, 
    get = get,
    setinv = setinv,
    getinv = getinv)
}

##################################################
##            Function cacheSolve               ##
##################################################

# Function cacheSolve reads the cached solution 
# from the parent environment. If the cached 
# does not exist, cacheSolve, calculates the 
# inverse of the matrix x$get()

cacheSolve <- function(x, ...) {
  # read the inverse of matrix x$get()
  # using x$getinv()
  inv <- x$getinv()
  
  # if the inverse is known (i.e. is not NULL)
  # return that value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if x$getinv() is NULL, it means that 
  # the inverse of x$get() is not known, hence it 
  # has to be calculated. Function solve(x$get(), ...)
  # calculates inverse of x$get()
  inv <- solve(x$get(), ...)
  
  # Inverse of x$get() is stored, so that it can be used 
  # later if needed.
  x$setinv(inv)
  
  # Return the inverse of x$get()
  inv
}