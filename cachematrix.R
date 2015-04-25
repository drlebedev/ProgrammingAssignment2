## Matrix solve file provides matrix inversion
## optimised implementation. Use makeCacheMatrix
## to prepare matrix data. 
## cacheSolve is optimised matrix inversion routine.
## First run will take the same time as regular
## solve(). Second run will use a cached value. 

## Prepare matrix data for inversion
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrix) im <<- inverseMatrix
    getInverseMatrix <- function() im
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Matrix inversion routine. First parameter
## is prepared matrix from makeCacheMatrix.
## Other parameters are matched standard
## solve function.
cacheSolve <- function(x, ...) {
    im <- x$getInverseMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    im
}
