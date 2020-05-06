## I have written the makeCacheMatrix and cacheSolve functions modeling them on the example 
## makeVector functions. 
## The makeCacheMatrix function creates a function object with 4 functions, and 2 matrices. 
## The cacheSolve functions takes as input the object created by makeCacheMatrix object and returns the 
## inverse of the matrix which is part of the makeCacheMatrix object. 
## makeCacheMatrix
## The makeCacheMatrix function basically has two matrices, the original, and its inverse, and 4 functions
## that allow the caller to get and set both the matrices.
## Both the set functions use the <<- assignment operator so that the function variables get updated and  
## lexical scoping ensures the caller gets the copy from the object. 

makeCacheMatrix <- function(origMat = matrix()) {
        
        invMat <- NULL
        set <- function(y) {
                origMat <<- y
                invMat <<- NULL
        }
        get <- function() origMat
        setInv <- function(invNew) invMat <<- invNew
        getInv <- function() invMat
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## cacheSolve
## The cacheSolve functions takes as input the object returned by the makeCacheMatrix fucntion
## It then interrogates the passed object to see if the inverse matrix has been cached already, and if 
## not, calculates the inverse, updates the makeCacheMatrix object with the inverse, and returns 
## the value to calling program.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInv()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        xMat <- x$get()
        yMat <- solve(xMat, ...)
        x$setInv(yMat)
        yMat
        
}