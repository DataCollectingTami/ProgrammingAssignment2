## The below functions will create a matrix object
## that can cache its inverse-- since matrix inversion
## is a costly computation

## makeCacheMatrix creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse<- function(inverse) inv <<-inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve computes the inverse of 
## the special matriz returned by makeCacheMatix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv

}
