## Functions (using lexical scoping) to manage
## a 'special' matrix
##  makeCacheMatrix - establishes the special matrix in the env
##      usage:  pass the special matrix makeCacheMatrix(m)
##
##  cacheSolve - creates an inverse matrix of the special matrix
##      usage: pass the special matrix cacheSolve(m)
##
##  Utils:
##      get() - retrieves special matrix ... m$get()
##      set() - internal usage only 
##      getsolve() - retrieves inverse of special matrix ... m$getsolve()
##      setsolve() - internal usage only 
## 
## solve(x) - Computes the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setsolve <- function(solve) m <<- solve   
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve - calculates the inverse of matrix 
##      usage:  pass the special matrix cacheSolve(m)
## Test if inverse matrx has been calculated previously, if so
## returns current inverse matrix


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
