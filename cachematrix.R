## Put comments here that give an overall description of what your 
## functions do version 1

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## Return error if the matrix is not invertible 
	
	 if (det(x) == 0 ) {return ("Error: matrix is not invertible")}
	
	## Return error if the library is not installed 
	if(!require('matrixcalc')) {
  		return ("Error: matrixcalc package not found")
	}
	
	## Reset the cached variable 
        m <- NULL
	
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	
        setInverse <- function(matrix.inverse) m <<- matrix.inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- matrix.inverse(data, ...)
        x$setInverse(m)
        m


}
