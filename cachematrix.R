## This function creates a special "matrix" object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL ## Use '<<-' to assign a value to an object in an environment
                                # different from the current environment.
        }
        get <- function() x
        setinverse <- function(inverse) inv_x <<- inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This will calculate the inverse of a matrix that was outputted by the makeCacheMatrix function. 
## If the matrix inverse has been calculated already then it will find it in the cache and return that 
## and do no calculations, saving time.
## Else, it will calculate, cache, and return results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("Please hold...getting cached inverse matrix")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inv_x)
                return(inv_x)
        }
}
