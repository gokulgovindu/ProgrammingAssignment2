## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function does the following
## - Sets the value of the matrix and gets the value of the matrix
## - Sets the value of the inverse and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The CacheSolve function first looks for a cached inverse and if found returns the cached inverse.
## If not found, it computes the inverse, caches it for future use and then returns the inverse.

cacheSolve <- function(x, ...) {
    ## Look for cached inverse
    i <- x$getinv()
    if(!is.null(i)) {
      message("Getting cached data")
      return(i)
    }
    
    ## Compute and cache inverse
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}

a <- matrix(rnorm(16), nrow=4, ncol=4)

ca <- makeCacheMatrix(a)

ainv <- cacheSolve(ca)



