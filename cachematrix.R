##The first function, makeCacheMatrix creates a matrix, which is really a list containing a function to
## set the value of the vector and
## get the value of the vector and
## set the value of the mean and
## get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL
        set <- function(y) {
                x <<- y
                CachedInv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInv <<- inverse
        getInverse <- function() cachedInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function calculates the inverse matrix created with the makeCacheMatrix function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        res <- x$getInvMatrix()
        if(!is.null(res))
                return(res)
        matrix <- x$get()
        res <- solve(matrix, ...)
        x$setInvMatrix(res)
        res
}
