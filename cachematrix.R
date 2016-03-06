## The makeCache Matrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setInverse <-function(inverse) i <<- inverse
   getInverse <-function() i
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## The cacheSolve function checks if the inverse has been calculated.
## If so, it retrieves the inverse from the cache.
## Else, it calculates the inverse.

cacheSolve <- function(z, ...) {
  ## Return a matrix that is the inverse of the matrix in 'z',
  ## whereby 'z' is an output of the function makeCacheMatrix.
  ## 'z' contains functions: set, get, getInverse, and setInverse
   i <- z$getInverse()
   if(!is.null(i)){
       message("getting cached data")
       return(i)
   }
   data <- z$get()
   i <- solve(data, ...)
   z$setInverse(i)
   i
}
