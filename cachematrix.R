## This file contains the function "makeCacheMatrix"
## to create an inverse matrix caching object.
## This object is to be used with function "cacheSolve"
## which computes the inverse only once and stores the
## result for faster retrieval in next uses

## makeCacheMatrix creates a inverse matrix caching object
## see inside for function usage

makeCacheMatrix <- function(x = matrix()) {
        ## The internal variable where the inverse is stored
        ## This is NULL until computed once
        i <- NULL
        
        ## set() function sets the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## get() function returns the value of the matrix
        get <- function() x
        
        ## setinverse() caches the inverse value
        setinverse <- function(inverse) i <<- inverse
        
        ## getinverse() computes the inverse matrix or returns the cached
        ## value if already computed
        getinverse <- function(...) {
                ## Check if we have a cached inverse matrix
                if(is.null(i))
                        ## There is no cached value, compute it
                        setinverse(solve(x,...))
                else
                        ## Inform the user a cached result is being used
                        message("getting cached data")
                ## Return inverse
                i
        }
        
        ## Return the list object with defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Shorthand for x$getinverse()
## All the workload is shifted to getinverse() function to improve
## encapsulation.
cacheSolve <- function(x, ...) {
        x$getinverse(...)
}
