## makeCacheMatrix creates a matrix object that can cache its inverse,
## and cacheSolve returns the cached inverse, or calculates if needed

## makeCacheMatrix returns a list of functions,
## that can get and store the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function()  x
  
  setinv <- function(inv) i <<- inv
  
  getinv <- function()    i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse if cached already, 
## othewise it calculates the inverse

cacheSolve <- function(x, ...) 
{
  i <- x$getinv()
  if(!is.null(i))             ##checks if inverse is cached
    return(i)
  
  data <- x$get()
  m <- solve(data)
  
  x$setinv <- i
  i
}
