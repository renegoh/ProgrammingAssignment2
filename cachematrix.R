## RG20150719 R Programming Assignment #2
## Assignment: Caching the Inverse of a Matrix
##    Part one: to create a matrix function that can cache the inverse
##    Part two: ## no need to check this--- to check if matrix is the same and inverse already calculated: 
##      - retrieve the cached inverse matrix if it exists
##      - else compute the inverse of the new, given matrix
## 
##
### Part one 
## this function is a group of functions that
## - creates the matrix
## - sets / gets the value of the matrix
## - sets / gets the value of the inverse
## - this also contains the cached inverse if it exists
## --- Note to self: it's like a class in C#

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize m - create before using; do it here so the entire function has access to it
  ## this is the cached inversed matrix (cm)
      cm <- NULL 
  
  ## this function accepts the input provided as the new matrix.
  ## because this is a NEW matrix, the cached matrix HAS to be cleared, therefore reset to NULL
      set <- function(y) {
          x <<- y
          cm <<- NULL
      }
  
  ## this function gets and returns the matrix x
      get <- function() 
        {x}
      
  ## this function sets the inverse as given; stores the inverse in the cache
      setinverse <- function(inverse) 
        {cm <<- inverse}
      
  ## this function gets and returns the inverse cached matrix
      getinverse <- function()
        {cm}
  
  ## this stores the functions 
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
}


### Part two 
## this is the actual function that inverses the given matrix
  ## Return a matrix that is the inverse of 'x'

  #  get stored inverse matrix (sm) : 
  #  if is valid, return it
  #  else calculate and store and return inverse

cacheSolve <- function(x, ...) {
  
  #  get stored inverse matrix (sm) : 
  sm <- x$getinverse()
  if (!is.null(sm)) 
  {# stored inverse matrix is valid, just return it.
    message ("getting cached matrix")
    return (sm)
  }
  else
  {# calculate and store and return inverse
    ## first, retrieve the matrix
      m <- x$get()
    ## then solve it and store the inverse in i
      i <- solve(m)
    ## set the inverse as well
      x$setinverse(i)
    ## return the inverse
      return (i)
  }

}
