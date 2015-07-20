## RG20150719 R Programming Assignment #2
## Assignment: Caching the Inverse of a Matrix
##    Part one: to create a matrix function that can cache the inverse
##    Part two: to check if matrix is the same and inverse already calculated: 
##      - if so, retrieve the cached inverse matrix
##      - else compute the inverse of the new, given matrix
## 
##
### Part one 
## this function is a group of functions that
## - creates the matrix
## - sets the value of the matrix
## - gets the value of the matrix
## - sets the value of the inverse
## - gets the value of the inverse
## - this also contains the cached inverse if it exists
## --- Note to self: it's like a class in C#

makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize m - create before using; do it here so the entire function has access to it
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

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get stored matrix and compare with given matrix
  #   if identical, get and check stored inverse
  #     if inverse is valid, return stored inverse
  #   if different matrix or no stored inverse
  #     calculate and store and return inverse
  
  sm <- x$get()
  if (!is.null(sm)){
    message ("this matrix is new")
    
    i <- solve(x)
    x$set(x)
    x$setinverse(i)
    return (i)
  }
  else
  {
    #compare with given matrix
    if (identical(sm,x)){
      message("same matrix as before, just retrieving cached inverse")
      i <- x$getinverse()
      if (!isnull(i)){
        message("getting cached inverse")
        return(i)
      }
      else
      {#cached inverse is missing?? just recalculate it
        i <- solve(x)
        x$set(x)
        x$setinverse(i)
        return (i)
      }
      
    }
  }

}
