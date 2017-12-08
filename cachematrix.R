## The aim of the following two functions is to simplify the potential time 
## costly calculation of the inverse of a matrix. If the inverse was once 
## calculated, it is stored in cache, so instead of calculating inverse of the 
## same matrix every time, the cached value will be returned. 


## The first function creates a list/vector of functions to put/store the 
## matrix, get the matrix, put/store the inverse, get the inverse:

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


## The second function checks in the previously generated list whether there is 
## the value of the inverse already stored: if it exists, it gives the message
## "getting cached data on matrix inverse" and assigned the value; if it does
## not exist in cache, the inverse is calculated and put/stored in cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data on matrix inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
