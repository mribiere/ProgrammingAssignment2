## Put comments here that give an overall description of what your
## functions do

## This function implement the set, get, setinverse and getinverse function for a matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        ## set the matrix
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        ## get the matrix
        get <- function() x
        ## set the inverse of the matrix
        setinverse <- function(inverse) invm <<- inverse
        ## get the inverse of the matrix
        getinverse <- function() invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return the inverse of a square invertible matrix either through the cache or compute it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        mat <- x$get()
        invm <- solve(mat, ...)
        x$setinverse(invm)
        invm
}
