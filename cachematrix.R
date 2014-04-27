## Calculate and return the inverse of an matrix and caches it automatically

makeCacheMatrix <- function(x = matrix())
{
    m1 <- NULL
    setMat <- function(y)
    {
        x <<- y
        m1 <<- NULL
    }
    getMat <- function() x
    setInvMatrix <- function(mat) m1 <<- mat
    getInvMatrix <- function() m1
    cacheSolve(list(setMat = setMat, getMat = getMat, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix))
}

## Calculates the inverse of an matrix passed on through the 'makeCacheMatrix' and caches it automatically

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInvMatrix()
    if(!is.null(invMatrix))
    {
        message("getting cached data")
        return(invMatrix)
    }
    data <- x$getMat()
    invMatrix <- solve(data, ...)
    x$setInvMatrix(invMatrix)
    invMatrix
}

## Test calls
## x <- matrix(rnorm(100), 10, 10)
## y <- makeCacheMatrix(x)
## y
