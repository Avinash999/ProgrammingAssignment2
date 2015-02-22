## The first function creates a special matrix object which will cache
## its inverse. 
## The second function checks if the inverse exists in the cache; if yes,
## then display from cache. If inverse doesnt exists in cache, then compute
## the inverse and store in the cache

## makeCacheMatrix function creates a special matrix which would cache
## its own inverse

makeCacheMatrix <- function(originalMatrix = matrix()) {
  
  ## Initialize the variable with NULL
  inv<-NULL
  
  ## Store the matrix in cache and initialize the variable to store its inverse
  ## with NULL
  setMatrix<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  ## retrieve the matrix
  getMatrix <- function() x
  
  ## set the inverse of the matrix in cache
  setInverse <- function(inverse) inv<<-inverse
  
  ## get the inverse of the matrix from cache
  getInverse <- function() inv
  
  matrix(set=setMatrix,get=getMatrix,setInverse=setInverse,
         getInverse=getInverse)
}


## cacheSolve function will check if inverse of the matrix exists in the cache.
## If it exists, then it will return the inverse from the cache
## If it doesn't exist in cache, then it will calculate the inverse and return
## the same. Also, it will store the same

cacheSolve <- function(x, ...) {
  
  ## retrieve the inverse from the cache
  inverse<-x$getInverse()
  
  ## Check if the inverse from the cache is NULL or not
  if(!is.null(inverse))
  {
    ## if the inverse is present in the cache, return the same and quit
    message("Retrieving from Cache")
    return inverse
  }
  
  ## if inverse does not exist in the cache, get the matrix
  data<-x$getMatrix()
  
  ## calculating the inverse of the matrix
  inverse<-solve(data, ...)
  
  ## store the calculated inverse in the cache
  x$setInverse(inverse)
  
  ## return the calculated inverse and quit
  inverse
}
