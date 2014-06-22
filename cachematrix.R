## The following two functions caches the inverse of a matrix
## in order to avoid recalculating the inverse 

## This function returns a list of functions that can be used to 
## implement a cache for a matrix. The set function sets a matrix 
## while get function return this matrix. The setinverse function 
## is used to cache the given inverse of the matrix while getinverse
## function returns this inverse

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL

    set <- function(n) {
        x <<- n
        xinverse <<- NULL
    }

    get <- function() x

    setinverse <- function(matinverse) xinverse <<- matinverse

    getinverse <- function() xinverse

    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## This function calculates the inverse of a cached matrix if it is
## not yet calculated and cache it. If it is already calculated it returns
## this calculated inverse

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    xinverse <- x$getinverse()
    if (!is.null(xinverse)) {
        return(xinverse)
    }
    mat <- x$get()
    matinverse <- solve(mat)
    x$setinverse(matinverse)
    matinverse
}
