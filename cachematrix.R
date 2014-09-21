## functions that cache the inverse of a matrix

## matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(matrix) {
        mat <<- matrix
        inv <<- NULL
    }
    get <- function() {
    	mat
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculate inverse of the special matrix returned by "makeCacheMatrix"
## If inverse has already been calculated and matrix hasn't been changed, 
## then "cachesolve" should retrieve inverse from cache.
cacheSolve <- function(x, ...) {
    mat <- x$getInverse()
    if( !is.null(mat) ) {
        return(mat)
    }
    data <- x$get()
    mat <- solve(data) %*% data
    x$setInverse(mat)
    mat
}