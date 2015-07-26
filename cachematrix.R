## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inverse <- NULL
 
    # Setter for the matrix   
    setMatrix <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    getMatrix <- function() x
    setInverse <- function(solve) inverse <<- solve ## solve(X) will return inverse
    getInverse <- function() inverse
    
    ## List of matrix containing given matrix set & get and
    ## inverse matrix get & set.
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)    
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  # If the inverse is already calculated, return it 
  if(!is.null(inverse)) { ## Checking whether cache already there or not.
    return(inverse)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$getMatrix()
  inverse <- solve(data, ...) ## solve(X) will return inverse
  
  # Cache the inverse
  x$setInverse(inverse)
}
