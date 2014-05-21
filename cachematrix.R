## Function Definitions 
## --------------------
## The functions may be seen as a conceptual definition for the methods of an object-oriented 
## object class for an invertable matrix.  The functions (aka methods) are as follows:
## 
## makeCacheMatrix - encapsulates the methods for the "invertable matrix" object class:
##     set - mutator (setter) method for instantiating an invertable matrix object in the cache.
##     get - accessor (getter) method for retrieving an invertable matrix object from the cache.
##     setinverse - mutator method for saving the matrix's inverse in the cache.
##     getinverse - accessor method for retrieving the matrix's inverse from the cache.
## cacheSolve - method for computing the inverse of the matrix
## ---------
## Test Case
## ---------
## The following R-commands serve to test the functions and illustrate their purpose:
## 
## mat <- makeCacheMatrix()              Creates a matrix object with its methods
## mat                                   Lists the methods in the object class
## mat$get()                             Shows the null matrix that is in the cache
## mat$set(matrix(c(-2,3,3,-4),nrow=2))  Puts an invertable 2X2 matrix in the cache       
## mat$get()                             Retrieves and displays the matrix
## cacheSolve(mat)                       Creates and saves the inverse using setinverse
## mat$getinverse()                      Retrieve and displays the inverse matrix
## mat$get() %*% mat$getinverse()        Matrix multiplication returns identity matrix 
##                                       proving that the inverse was found
## -------------------------------------------------------------------------------
## --------------------------------------------------------------------------------
## makeCacheMatrix - Creates a matrix and saves it in the cache.  Includes internal 
##  functions for saving and retrieving the matrix and its inverse.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## -----------------------------------------------------------------------
## cacheSolve - Computes the inverse of a matrix and saves it in the cache
## -----------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    inversedata <- x$get()
    m <- solve(inversedata, ...)
    x$setinverse(m)
    m
}
