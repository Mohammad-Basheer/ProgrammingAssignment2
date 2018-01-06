## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function create special object with four functions to set and get a matrix and its inverse 
## cacheSolve function used to get the inverse of the matrix of object created with makeCacheMatrix using solve function if not calculated before other wise get from cach
## note: passing new matrix to the makeCacheMatrix object resets its inverse

## Write a short comment describing this function
## set function set new matrix value and reset the inverse value to be NULL
## get function return the matrix value
## setsolve set inverse matrix value
## getsolve get inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(xinv) m <<- xinv
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## by calling getsolve get inverse matrix value , if null use solve to get its value , if exist reutrn it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    #below line can be modified for debuging perpose
    #message("getting cached data") 
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
