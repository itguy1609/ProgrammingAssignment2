## The below function makecacheMatrix creates a matrix 'x' 
## And also caches the inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  # Assign the inverse to NULL
  inv_x <- NULL
  
  # set function resets the inverse matrix (inv_x) to NULL
  set <- function(y) {
    # assign the value of y to x
    x <<- y
    # Set the inv_x to NULL when the set function is called
    inv_x <- NULL
    
  }
  
  # get function returns the set matrix x above
  get <- function() x
  
  # setmatrixInverse assigns the inverse of matrix value to inv_x
  setmatrixInverse <- function(solve)   inv_x <<- solve
  
  # getInverse returns matrix inverse inv_X
  getmatrixInverse <- function() inv_x
  
  # Create a list containing all the functions created above
  list(set = set, get = get, setmatrixInverse = setmatrixInverse, getmatrixInverse = getmatrixInverse)  
  
}


## The function calculates the inverse of the matrix returned by makeCacheMatrix
## If the inverse was previuosly calculated then it should return the cached value 

cacheSolve <- function(x, ...) {
  
  # get the value of inverse matrix
  inv_x <- x$getmatrixInverse()
  
  # if the value is NOT NULL then the inverse is calculated previously and cached
  # So retrun the cached inverse matrix value
  if(!is.null(inv_x)){
    message("getting cached data")
    return(inv_x)
    
    # If the value of Inverse matrix is NULL, then get the new matrix value x
    # and calculate the inverse and return the inverse matrix inv_x
    message("newly calculating data")
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setmatrixInverse(inv_x)
    inv_x
  }
  