## Function creates a special "matrix" object that can cache matrix inverse
## It is really a list containing a function to:
##  1. set the value of matrix x provided as argument (set)
##  2. get the value of the matrix x (get)
##  3. set the value of matrix inverse (setMatrixInverse)
##  4. get the value of matrix inverse (getMatrixInverse)

makeCacheMatrix <- function(x = matrix()) {

    ## Initially set matrix inverse cache to NULL
    matrixInverseCache <- NULL
    
    ## define 'set' function - set the value of new matrix and clean the cache
    set <- function(newMatrix) {
        
        ## Only store new matrix and reset cache if the newly provided matrix
        ## is different from the already stored matrix 'x'
        if (!all(x == newMatrix)) {
            message("Setting new matrix and reseting cache to NULL")            
            x <<- newMatrix
            matrixInverseCache <<- NULL            
        }
        
        message("Provided matrix equal to already stored matrix")
        message("Leaving calculated inverse cache intact!")
    }
    
    ## define 'get' function - get the value of the matrix x
    get <- function() {
        x
    }
    
    ## define 'setMatrixInverse' function - set matrixInverseCache to the value
    ## returned by cacheSolve function
    setMatrixInverse <- function(matrixInverse) {
        matrixInverseCache <<- matrixInverse
    }
    
    ## define 'getMatrixInverse' function - get matrix inverse value
    ## from the cache (value of matrixInverseCache variable)
    getMatrixInverse <- function() {
        matrixInverseCache
    }
    
    ## define the list of provided functions of this special object
    list(set = set, 
         get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)    
}


## Function computes the inverse of the matrix stored in the special "matrix"
## object created by the mackeCacheMatrix function. 

cacheSolve <- function(x, ...) {
    
    ## Retrieve inverse value from cache of the special "matrix" object
    matrixInverse <- x$getMatrixInverse()
    
    ## If value is not NULL - return value stored in cache
    if (!is.null(matrixInverse)) {
        message("Retrieving cached inverse of the matrix")
        return(matrixInverse)
    }
    
    ## If value is NULL - compute new inverse and store it in cache
    
    ## Retrieve matrix
    matrix <- x$get()
    
    ## Compute inverse
    matrixInverse <- solve(matrix)
    
    ## Store computed inverse in special object's cache
    x$setMatrixInverse(matrixInverse)    
    
    ## Return a matrix that is the inverse of 'x'
    matrixInverse
}
