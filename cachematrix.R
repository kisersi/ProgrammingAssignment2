## This function creates a special "matrix" object that will cache its inverse and 
## returns a list of functions that will be used by cacheSolve to get or set
## the inverted matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) { ## creates the matrix in the working environment
                x <<- y
                inv_x <<- NULL ## Use '<<-' to assign a value to an object in an environment
                                # different from the current environment.
        }
        get <- function() x ## Gets the value of the matrix
        setinverse <- function(inverse) inv_x <<- inverse ## Inverts the matrix and stores it in the cache
        getinverse <- function() inv_x ## gets the inverted matrix from cache
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse) ## Returns the created functions to the console
}


## This will calculate the inverse of a matrix that was outputted by the makeCacheMatrix function. 
## If the matrix inverse has been calculated already then it will find it in the cache and return that 
## and do no calculations, saving time.
## Else, it will calculate, cache, and return results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse() ## This will attempt to get the inverse of the matrix if it is stored in the cache
        if (!is.null(inv_x)) { ## return inverted matrix from the cache if it exists
                message("Please hold...getting cached inverse matrix") ## Displays the message
                return(inv_x) ## Displays the Matrix in console
        } else {
                inv_x <- solve(x$get()) ## Matrix will be created since it does not exist
                x$setinverse(inv_x) ## set the inverted matrix in cache
                return(inv_x) ## Displays the Matrix in the console
        }
}
