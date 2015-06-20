## These functions creates a special "matrix" object 
## that can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    matrix.inverse <- NULL
    set <- function(y){
        x <<- y
        matrix.inverse <<- NULL
        
    }
    
    get <- function() x
    
    setInverse <- function(x) matrix.inverse <<- x
    
    getInverse <- function() matrix.inverse
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matrix.inverse <- x$getInverse()
    if(!is.null(matrix.inverse)){
        message("getting cached inverse matrix")
        return(matrix.inverse)
    }
    data <- x$get()
    matrix.inverse <- solve(data)
    x$setInverse(matrix.inverse)
    matrix.inverse
}