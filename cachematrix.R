## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

## This function returns a list of functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of inverse 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function() x
  seti<-function(inverse) i<<-inverse
  geti<-function() i
  list(set=set,get=get,seti=seti,geti=geti)

}


## This function returns the inverse of the matrix. It checks if the inverse already exists. 
## If it does, the function gets the cached value of the inverse and return it. 
## If the inverse does not exist, it computes the inverse, sets the value and then returns it.

cacheSolve <- function(x, ...) {
  i <- x$geti()
  if(!is.null(i))  {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$seti(i)
  i
  ## Return a matrix that is the inverse of 'x'
}
