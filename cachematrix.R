## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## This is a pair of functions that cache the inverse of a matrix.

## THe function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    
    ## If the inverse has been already calculated    
    if(!is.null(inver)) {
    
      ## Get it from the cache to skip computation
      message("getting cached data")
      return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    return(inver)
}
