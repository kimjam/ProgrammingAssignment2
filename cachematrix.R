## makeCacheMatrix creates a "matrix" object that can 
## cache its inverse.
## cashSolve computes the inverse of the "matrix" returned 
## by makeCacheMatrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set allows the user to set a new value for the special "matrix" object
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get allows cacheSolve to retrieve the special "matrix" object
    get <- function() x
    
    # setinv sets the value of m to the inverse of the inputted matrix
    setinv <- function(solve) m <<- solve(x)
    
    # getinv allows cacheSolve to get the inverse of the special "matrix" object
    getinv <- function() m
    
    # creates a list of the functions cacheSolve can call
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes and returns the inverse of the special "matrix" object returned by
## makeCacheMatrix. If the inverse has already been calculated, cacheSolve will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

    # gets the inverse of the special "matrix" object returned by makeCacheMatrix
    m <- x$getinv()
    
    # tests to see if the inverse of the returned "matrix" object has already been caculated or not
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # sets data equal to the "matrix" object created by calling the get function
    data <- x$get()
    
    # solves for the inverse of the matrix
    m <- solve(data, ...)
    
    # sets the inverse of the matrix that can be retrieved from the cache if cacheSolve is called with the same matrix
    x$setinv(m)
    
    # returns the inverse of the matrix
    m
}
