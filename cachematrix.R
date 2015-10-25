##Matrix inversion is usually a costly computation and these functions create a special matrix with getInverse and setInverse to cache the matrix inverse. 


## This function creates a special matrix. Special matrix can be used as x$getInverse. This function uses matrixcalc package
## install the package and load the library with library(matrixcalc) 
makeCacheMatrix <- function(x = matrix()) {

## Return error if the matrix is not invertible 
	
		## Return error if the matrix is not invertible 
	
	 if (det(x) == 0 ) {return ("Error: matrix is not invertible")}
	
		
	## Reset the cached variable 
        m <- NULL
	
      ## this resets the environment cache for the matrix  
	 set <- function(y) {
                x <<- y
                m <<- NULL
        }
      ## returns cached matrix  
	 get <- function() x

	
      ## calculates inverse  
	setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##This function returns the inverse, If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
 	    m <- x$getInverse()
	
	 ## returns value from cache if exists 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
       
	 data <- x$get()
	 ## calculates inverse of the matrix 
        m <- solve(data, ...)
        x$setInverse(m)
	 ## Return a matrix that is the inverse of 'x'
        m



}
