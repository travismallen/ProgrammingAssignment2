## The following script provides a pair of functions that cache the inverse of
## a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        }

    get <- function() x
    
    InvMatrix <- solve(x)
    
    setInvMatrix <- function(InvMatrix) m <<- InvMatrix

    getInvMatrix <- function() m

    list(set = set, get = get, setInvMatrix = setInvMatrix,
      getInvMatrix = getInvMatrix)
}


## This function computes the inverse of the special "matrix" if it does not
## exist in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()

    if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
    this_m <- matrix()
    
    this_m <- x$get()

    m <- solve(this_m)

    x$setInvMatrix(m)

    m

}
