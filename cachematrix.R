## The pair of functions defined below can be used to compute the inverse of any 
## matrix and cache it so that it can be reused when required without repeating 
## the entire computation.


## The makeCacheMatrix function creates a special "matrix" object "inv" that can  
## cache the inverse of any given invertible matrix "x". 

## This function returns a list of four functions which are used to set the 
## matrix, get the matrix, set the inverse of the given matrix and get the 
## inverse of the given matrix. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
            
      }
      set(x)
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, 
           setinv = setinv, getinv = getinv)
}


## The cacheSolve function checks if the inverse of a given matrix is already 
## computed and stored in the cache, and if it is, it returns the inverse matrix
## from the cache. If the inverse is not previously cached this function computes
## it and caches this inverse for future usage amd returns the inverse.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinv()
      if (!is.null(inverse)){
            message("Getting cached data")
            return(inverse)
      }
      matrix <- x$get()
      inverse <- solve(matrix)
      x$setinv(inverse)
      inverse
}
