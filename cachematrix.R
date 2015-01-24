## I have written two functions :
##    1. makeCacheMatrix()
##       - It can be used to get a matrix, set matrix, 
##         calculate inverse and set it, and allows to get the inverse.
##    2. cacheSolve()
##       - It makes a check for existence of inverse of a mtrix in cache.
##         It calculates inverse only if its not present in cache.


## The function 'makeCacheMatrix' takes a matrix as input.
## It provides 4 capabilities :
## 1. Setting values in matrix
## 2. Getting matrix 
## 3. Setting inverse of matrix
## 4. Getting inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{
  ## Sets inverse as null
    m<-NULL
    
  ## Sets the values of the matrix
    setmatrix<-function(y)
    {
      x<<-y
      m<<-NULL
    }
  
  ## Gets the values of the matrix
    getmatrix<-function() x
  
  ## Sets the inverse in m
    setinverse<-function(solve) m<<- solve
  
  ## Returns the calculated inverse 
    getinverse<-function() m
    list(setmatrix=setmatrix, getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
  }

## The function 'cacheSolve' takes a matrix as input.
## It checks if its inverse exists in the cache or not.
## If inverse is found in cache, it returns the inverse from the cache.
## If not found, it calculates the inverse and then returns it.
## In short , Return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) 
{
  ## Gets the inverse of matrix
    m<-x$getinverse()

  ## Returns the inverse if its already there in the cache
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
  
  ## If inverse is not in cache, it gets the matrix
    matrix<-x$getmatrix()
  
  ## inverse is found and stored in m
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
