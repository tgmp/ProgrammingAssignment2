## In order to avoid potentially time consuming calculations such as matrix 
## inversion, the following functions take advantage of R's use of lexical 
## scoping to store the value of the inverse of a matrix in the cache and 
## retrieve it instead of performing the same computation several times.

## The makeCacheMatrix() function takes an invertible matrix as an argument and 
## returns a list of ("getter and setter") functions, which is to be passed as 
## an argument to cacheSolve() in order to cache the inverse of the input 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The cacheSolve() function takes as its argument 'makeCacheMatrix'-type 
## objects and returns the inverse of the matrix that was passed as the argument
## to makeCacheMatrix.
## First, it checks if the inverse has already been stored in the cache. If this
## is the case, it returns the stored value. Otherwise, it calculates and
## returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
