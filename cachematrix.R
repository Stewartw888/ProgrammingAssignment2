##The first function, makeCacheMatrix creates a matrix, which is really a list containing a function to
## set the value of the vector and
## get the value of the vector and
## set the value of the mean and
## get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL
        set <- function(y) {            ## Reset matrix
                x <<- y                 ## Assign matrix to x
                CachedInv <<- NULL      ## Cache to NULL
        }
        get <- function() x
        setInv <- function(inverse) cachedInv <<- inverse
        getInv <- function() cachedInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## The following function calculates the inverse matrix created with the makeCacheMatrix function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        res <- x$getInvMatrix()         
        if(!is.null(res))      {           ## If result has been calcualted previously
        message("getting cached data...") ## Patience, please...
                return(res)               ## Return existing output if previously calculated
        }
        matrix <- x$get()                 ## If not available, fetch matrix
        res <- solve(matrix, ...)         ## If not available, calc result
        x$setInvMatrix(res)               ## Calc inverse matrix 
        res                               ## Print output
}

