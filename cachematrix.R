## The 2 functions were based on the examples of the Assignment. 

## The first function is going to take the matrix and it is able to catch when there
## is a inverse result. For that, i added two levels of parameters: 1) controls the way the
## function works, and 2) it's going to have the capacity to modify variables. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- NULL}
  getInverse <- function() {inv}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is going to compute the inverse of the matrix; it is able to check
## if the inverse has been calculated, that is why it is used the "if" function. If there
## is no inverse, it is able to calculate the matrix and set its value.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
