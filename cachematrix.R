## The functions below are used create and cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## computing it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        myMatrix <- x
        myInverseMatrix <- NULL
        
        ## sets the value of myMatrix
        set <- function(y = matrix()) {
                myMatrix <<- y                   
        }
        
        ## gets the value of myMatrix
        get <- function() {
                myMatrix
        }
                
        ## sets the value of myInverseMatrix
        setInverse <- function(y = matrix()) {
                myInverseMatrix <<- y
        }
                
        ## gets the value of myInverseMatrix
        getInverse <- function() {
                myInverseMatrix
        }
        
        ## returns a list of the functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()         ## get the inverse from the matrix x         
        if(!is.null(i)) {           ## if there is a cache (i.e. if cache is not null)
                message("getting cached data") 
                return(i)           ## return the cache
        }
        data <- x$get()             ## if the cache was null, get the matrix x
        i <- solve(data, ...)       ## compute the inverse of x with solve()
        x$setInverse(i)             ## cache the result
        i                           ## Return a matrix that is the inverse of 'x'
}
