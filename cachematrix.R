## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {  #  it takes an argument x of type matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve # the <<- operator can be used to assign a value to an object in an environment that is different from the current environment. 
    getInverse <- function() m
    list(set = set, get = get,    # it returns a list with 4 list items  
         setInverse = setInverse, # they are actually 4 functions wrapped in a list
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    m <- x$getInverse()         # query the x matrix's cache         
    if(!is.null(m)) {           # if there is a cache
        message("getting cached data") 
        return(m)               # just return the cache, no computation needed
    }
    data <- x$get()             # if there's no cache
    m <- solve(data, ...)       # we actually compute them here
    x$setInverse(m)             # save the result back to x's cache
    m                           # Return a matrix that is the inverse of 'x'  
}
