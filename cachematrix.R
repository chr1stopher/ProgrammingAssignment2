## This file contains the definition of two functions aiming at optimising the
## computation time needed to inverse matrices by storing the result of such 
## inversion. To do so, matrices are stored together with their inverse (if it
## was computed) in a new format (see the makeCacheMatrix function). The cacheSolve
## function first checks whether the inverse of the matrix has already been computed
## and, if so, doesn't compute the inverse but retrieves it from the cache. 


## The makeCacheMatrix function embeds four functions. One can enter a new 
## matrix by using the 'set' function (note that it will reinitialise the inverse matrix).
## The 'get' and 'getinv' functions respectively allow the user to retrieve the matrix
## and its inverse. Finally, 'setinv' is the function used by the cacheSolve function
## to save the inverse of the orginal matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) inv <<- y
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## The cacheSolve function aims at inverting the matrix it is provided with as an 
## argument. To do so, it first checks whether this computation has already been
## made, and if so, gets the result from the cache. Otherwise it does the inversion
## and prints the result after having cached the result in case the inversion has
## to be performed again in the future.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse of your matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## Example
## m <- matrix(runif(9),3,3)    defines a 3x3 random matrix
## x <- makeCacheMatrix(m)      put the matrix in the CacheMatrix format
## cacheSolve(x)                computes the inverse of m and stores it
## cacheSolve(x)                retrieves the inverse from the cache
## x$get() %*% cacheSolve(x)    checks that the inverse is well computed

