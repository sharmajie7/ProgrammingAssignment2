##     makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## Caching the inverse of a matrix!

makeCacheMatrix <- function(x = matrix()) {
        
        ##iniatilizing the matrix
        m <- NULL
        ##set the matric=x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##get the matrix
        get <- function() x
        ##Set the function to inverse
        setinv <- function(solve) {
                m <<- solve
        }
        getinv <- function()
        {    m 
        }
        
        
        ##Return a list of all the methods used
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
        
}


## Calculate Inverse of a matrix

cacheinv <- function(x, ...) {
        ##Obtain the matrix
        m <- x$getinv()
        ##Checking if the matrix is empty or not
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ##store matrix 
        data <- x$get()
        ##Solve function solves the equation a %*% x = b for x,
        ##where b can be either a vector or a matrix.
        m <- solve(data, ...)
        x$setinv(m)
        ##print the matrix
        m
}
