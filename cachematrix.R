## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix (set...)
## get the value of the matrix (get...)
## set the value of the inverse (setinverse...)
## get the value of the inverse (getinverse...)


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}



## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix.
## It first checks to see if the inverse already exists, in which case it gets it from the cache and
## skips the rest. Otherwise, it computes it and it sets the value of the inverse in the cache (x$setinverse(inverse)).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- inverse(data, ...)
        x$setinverse(inverse)
        inverse
}




