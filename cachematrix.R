## Here, we define 2 functions that (makeCacheMatrix and cacheSolve) 
##that can chache potentially time-consuming computations. These computations
##helps us to get the inverse of a matrix.

## This function creates a list containing a list to set and get the
## value of matrix, to set and get the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() { x }
   setInverse <- function(inverse)
   {
     inv <<- inverse
   }
   getInverse <- function() { inv }
   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
   
}


##This calculates the inverse of the matrix and sets the value of the
##inverse in the cache via the cacheSolve function.

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
       message("getting cached data")
       return(inv)
     }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
