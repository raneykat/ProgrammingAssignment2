## Katherine Raney October 2014
# Introduction to R programming, Coursera Data Science Track
# Programming Assignment 2

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
        {
        #This function takes a matrix and returns a list of set and get functions

        i <- NULL 
        set <- function(y) 
                {
                        x <<- y
                        i <<- NULL
                }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the `solve`
#function in R. For example, if `X` is a square invertible matrix, then
#`solve(X)` returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #basically, use getmat to determine if the matrix has changed (???)
        # then use getinv to see if the inv is cached already
        # and if it is return it and if not then do the inverse and cache it
        i <- NULL
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
    
        
}
