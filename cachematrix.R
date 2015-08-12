## This R script is for R Programming course's second programming assignment

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inversemat <- NULL
    
    set <- function(x) {
        x <<- y
        inversemat = NULL
    }
    
    get <- function() x
    
    setInverse <- function(invvar) inversemat <<- invvar
    
    getInverse <- function() inversemat
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inversemat <- x$getInverse()
    
    if(!is.null(inversemat)) {
        print("Found cached matrix. Returning cached matrix!!")
        return(inversemat)
    }
    
    data <- x$get()
    
    inversemat <- solve(data)
    
    x$setInverse(inversemat)
    
    inversemat
}
