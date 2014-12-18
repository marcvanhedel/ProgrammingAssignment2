## =================================================================
## =================================================================
## = These functions are used to calculate the inverse of a matrix x
## = and cache the result.
## = If the inverse matrix has already been calculated, 
## = it is returned without the need to recalculate.
## = First call makeCacheMatrix(x) with x as matrix
## = Then call cacheSolve(x)
## =================================================================
## =================================================================

## =================================================================
## = makeCacheMatrix (x = matrix())
## = function to store and retrieve a matrix and its inverse
## = functions
## =    get() - gets the original matrix
## =    set(x) - stores a matrix
## =    getInvMatrix() - retrieves the cached inverse matrix
## =    setInvMatrix(x) - stores inverse matrix in m
## =================================================================
makeCacheMatrix <- function(x=matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setInvMatrix <- function(invMatrix) m <<- invMatrix
    
    getInvMatrix <- function() m
    
    ## return a list of functions as an R object
    list (set=set, 
          get=get, 
          setInvMatrix=setInvMatrix, 
          getInvMatrix=getInvMatrix)
}

## =================================================================
## = cacheSolve(x)
## = Function to solve the matrix. (calculate inverse)
## = If the matrix already has been solved, no need to recalculate
## = Otherwise solve the matrix and store it
## =================================================================

cacheSolve <- function(x) {
    m <- x$getInvMatrix()
    if(!is.null(m)){
        ## Ha! No need to recalculate it        
        message("Getting cached data... No need to recalculate...")
        return(m)
    }
    else {
        message("We need to solve this matrix... Calculating...")
        data <- x$get()
        m <- solve(data)
        x$setInvMatrix(m)
        message("Got it!")
        return(m)
    }
}