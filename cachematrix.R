## Here are two functions, makeCacheMatrix and cacheSolve, which work together
## to invert a square matrix by one of two methods. The first is to check 
## whether the matrix has been inverted previously, and, if so, return the 
## inverse from a cache. The second is to invert the matrix using the solve() 
## function, and store the inverse to the cache should it be required in future

## makeCacheMatrix wraps four separate functions around a matrix x, and is used
## to store the matrix, and also its inverse, in a cache for future use

makeCacheMatrix <- function(x = matrix()) {     
        InV <- NULL
        set <- function(y) {
                x <<- y
                InV <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) InV <<- inverse
        getinverse <- function() InV
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is used to invert the matrix stored in makeCacheMatrix via one of
## the two methods described in the introduction. It will make use of the
## property that the only way the matrix can have its inverse stored in the
## cache is if it has been calculated previously

cacheSolve <- function(x, ...) {        
        InV <- x$getinverse()
        if(!is.null(InV)) {
                message("getting cached data")
                return(InV)
        }
        data <- x$get()
        InV <- solve(data, ...)
        x$setinverse(InV)
        InV
        
}