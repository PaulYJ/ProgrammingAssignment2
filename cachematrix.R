## This function first creates a matrix object of the input matrix, 
## then check if the inverse was calculated. Return the calculated 
## matrix if that is available or calculate the inverse.

## This function create a set of functions that store the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x<<-y
        i<<-NULL
    }
    get <- function()x
    setSolve <- function(inverse)i <<- inverse
    getSolve <- function()i
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function check if the inverse was calculated.
## If not, the new inverse will be calculated using solve() function.
## Not all matrix are solvable!

cacheSolve <- function(x, ...) {
    i<- x$getSolve()
    if (!is.null (i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setSolve(i)
    i
}
