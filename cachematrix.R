## Put comments here that give an overall description of what your
## functions do

## The function create a object with four methods.
## set method store the original matrix.
## get method recover the original matrix.
## setmatrixinv method store the inverse matrix.
## getmatrixinv method recover the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrixinv <- function(matrixinv) m <<- matrixinv
    getmatrixinv <- function() m
    list(set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv)
}


## The function calculates the inverse matrix and store in the cache.
## If the function is executed more than once, the inverse array is retrieved from the cache.
## If there is a change in the original matrix (m == NULL), a new calculation of the inverse matrix is performed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrixinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrixinv(m)
    m
}


