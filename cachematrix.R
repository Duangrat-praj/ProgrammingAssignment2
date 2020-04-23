## There are 2 functions. One is for computing the inverse of a matrix and 
## the other one is for caching its inverse

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function calculates the inverse of the matrix from the
## function makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
                m <- x$getInverse()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setInverse(m)
                m
        ## Return a matrix that is the inverse of 'x'
}
