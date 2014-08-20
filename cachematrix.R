## Define a rectangular matrix as the argument x, e.g., matrix(1:4,2,2)

makeCacheMatrix <- function(x = matrix()) {
   
    # NULL if no argument is found
    m <- NULL
    
    # setting and storing the matrix
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    getMatrix <- function() x
    
    # set and cache the value of the inverse
    setInverse <- function(solve) m <<- solve
    
    # get and cache the value of the inverse
    getInverse <- function() m
    
    # list used functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
         getInverse = getInverse)
}

# This function calculates the inverse of the matrix created with makeCacheMatrix

cacheSolve <- function(y, ...) {
    
    # get the cached value of the inverse
    m <- y$getInverse()
    
    # if the value is found, it is returned
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if the value is not found, caclulate the inverse, store the vale and return it
    data <- y$getMatrix()
    m <- solve(data)
    y$setInverse(m)
    m
}