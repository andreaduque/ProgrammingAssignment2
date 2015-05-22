## This function is able to cache potentially time-consuming computations. 
## The assignment is about writing a pair of functions that cache the inverse of a matrix.

mmakeCacheMatrix <- function(x = matrix()) {
        m <- NULL                             # initialize the object for the matrix          
        get <- function() x                   # This function creates a special "matrix" object that can cache its inverse.
        setInv <- function(inv) m <<- inv     # definition of the functions
        getInv <- function() m                
        list(get=get, setInv=setInv, getInv=getInv) # list of functions (output of the function)
}

## cacheSolve: Check if the input matrix is already inverted. 
## If so, the matrix is just cached, otherwise the inverted matrix is calculated.
       
cacheSolve <- function(x, ...) {    # his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
        m <- x$getInv()             # get the cached inverted matrix of x and assign it to m object
        if(!is.null(m)) {           # executed if there is a cached inverted matrix 
        message("Getting cached data...")
        return(m)
        }
        else{                   # executed if there is no cached inverted matrix
            message("No cached data...Calculating...")
            data <- x$get()     # store in data object the matrix
            m <- solve(data)    # calculate the inverse matrix 
            x$setInv(m)         # set the value of the inverse matrix
            return(m)
  } 
}      