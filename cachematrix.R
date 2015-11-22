## Put comments here that give an overall description of what your
## functions do

## The following function creates a list of special matrix functions:
## get() returns the matrix
## set(m) sets the matrix values to the values in m
## getinverse() retrieves the inverse of the matrix
## setinverse(m) sets the inverse of the matrix to the values in m

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # create the set function (sets x to provided y argument)
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    # create the get function (returns x)
    get <- function() x
    # create the setinverse function (sets inverse to provided inv argument)
    setinverse <- function(inv) i <<- inv
    # create the get inverse function (returns the stored inverse value)
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function returns the inverse of the provided matrix created
## with the
## makeCacheMatrix function, first checking to see of the mean has previously
## been calculated and cached. If found in the cache, the result is returned and
## computation is skipped; otherwise, the inverse of the matrix is calculated
## and stored in the cache matrix.

cacheSolve <- function(x, ...) {
    # check the cacheMatrix object to see if an inverse has already been stored
    # for this matrix
    i <- x$getinverse()
    # if a stored inverse is found, then return
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # else, calculate the inverse of the matrix in the cacheMatrix object
    data <- x$get()
    i <- solve(data)
    # store the inverse in the cacheMatrix
    x$setinverse(i)
    # return the inverse
    i
}
