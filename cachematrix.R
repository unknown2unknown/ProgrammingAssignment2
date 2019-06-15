## Matrix inversion is computationally intensive and is beneficial to cache the 
## inverse of a matrix. Two functions are written; (1) firstly create a matrix 
## that cache the inverse of a matrix (2) to compute the inverse of the matrix.

## makeCacheMatrix is a function that creates a matrix and cache its' inverse


makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function (y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invm <<- inverse
        getInverse <- function() invm
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


   
## cacheSolve is a function that computes the inverse of the matrix returned by
## makeCacheMatrix. If the inverse has already been cached, cacheSolve will 
## retrieve the inverse of the matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getInverse()
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setInverse(invm)
        invm
}
