## Below are a couple of functions that are used to create a special object that
## saves the matrix and cache its inverse.
## This function create a special "matrix" object that can cache its reverse side.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This capacity figures the converse of the special "matrix" 
## made by makeCacheMatrix above. Assuming that the converse has 
## proactively been determined (and the network has not changed), 
## then it ought to recover the opposite from the reserve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
