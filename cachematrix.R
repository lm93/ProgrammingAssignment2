## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## setting the value of new matrix according to argument
        ## This segment assures that the matrix cannot be changed while keeping the old inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## function to get the matrix
        ## Example code:
        ## x <- matrix(1:4,2,2)
        ## L <- makeCacheMatrix(x)
        ## L$get()
        ## The last line gives the same result as printing out x
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        ## List of arguments for the resulting variable
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## If the inverse exists, it will be returned from cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## If the inverse hasn't been cached, it will be calculated
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
