## This functions create an object that can computes the inverse of the special "matrix"
## and use cache, if inverse matrix already been computed on a previous steps 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    xi <- NULL
    
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setinv <- function(minv) xi <<- minv
    getinv <- function() xi
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    
    if (!is.null(xinv)) {
        print("return cached data")    
        return(xinv)
    }

    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
