## The pair of functions allows us not two calculate inverse of matrix twice. 
## Ones the inverse of some matrix is calculated, it is stored in the cache.
## If we need an inverse of specific matrix, functions check cache first.
## If the inverse had not been calculated yet, functions do the job.


## Creates a list, which writes input matrix and default NULL for its inverse in "set"; 
## stores the input matrix in "get";
## could store inverse of the input matrix in "getinv";
## "setinv" is used in the second function

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv_x <<- inverse
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Calculates the inverse of the matrix created above.First checks the cache. 
## If the inverse matrix had not been calculated yet, calculates it
## and writes it in the "inv_x" in the cache via the "setinv" function.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x   ## Return a matrix that is the inverse of 'x'
}
