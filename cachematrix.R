## Two function that cache and solve the inverse of a matrix.
## 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cache_inv <- NULL
  
  ## The matrix set method
  set <- function( matrix) {
    x   <<- matrix
    cache_inv <<- NULL
  }
    
  ## The get method
  get <- function() {
    x
  }
  
  ## The inverse set method
  setInverse <- function (inverse) {
    cache_inv <<- inverse
  }
  
  ## The inverse get method
  getInverse <- function () {
    cache_inv
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  

}

# cacheSolve: This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated 
# and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_matrix_x <-x$getInverse()
    
    # Return the cached matrix
    if(!is.null(inv_matrix_x)) {
      message("getting cached data")
      return(inv_matrix_x)
    }
    
    # Set data getting the matrix object
    matrix_x <- x$get()
    
    inv_matrix_x <- solve(matrix_x,...)
    
    # Set the inverse matrix to the object
    
    x$setInverse(inv_matrix_x)
    
    # Return inv_matrix_x the matrix
    inv_matrix_x
}

 
