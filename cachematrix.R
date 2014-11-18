## The function makeCacheMatrix creates a special "matrix", which is a 
## list containing a functions that:
## 1.set the input matrix
## 2.get the input matrix
## 3.compute the inverse matrix
## 4.get the inverse matrix
##
## The function cacheSolve calculates the inverse of the special "matrix" 
## returned by the function makeCacheMatrix. First, it checks to see if the
## inverse matrix has already been calculated. In this case, it gets the inverse
## matrix from the cache and skips the computation. Otherwise, it calculates the 
## inverse matrix with the solve function and sets it in the cache using the 
## setinvm function.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm <- function(solve) invm <<- solve
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}

##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}

