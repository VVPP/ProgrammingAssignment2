## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the value of matrix
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of matrix
  get <- function () x
  
  # set the value of inverse
  set_inverse <- function(inv_input) inv <<- inv_input
  # get the value of inverse
  get_inverse <- function () inv
  
  
  
  # return a list of all the above function
  list(set = set, get = get,
       set_inverse = set_inverse, 
       get_inverse =  get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # checking the inverse
  # if so, we getting cache directly
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inversed")
    return (inv)
  }
  
  # else we get the matrix
  data <- x$get()
  # and calculate the inverse
  inv <- solve(data, ...)
  # cache the inverse of the matrix
  x$set_inverse()
  # return the result
  inv
}
