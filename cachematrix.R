## Caching the invers of an Object, in this case its a Matrix.
##First we create this "special matrix" with makeCacheMatrix
## and in the last funtion we use "solve function" to work with the inverse.

## Create a Matrix, that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invM<- NULL
  set<- function(y){
    x <<-y
    invM <<- NULL
  } 
  get <- function () x
  setInv <- function(inverse) invM <<- inverse
  getInv <-function()invM
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <-x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data1 <-x$get()
  m <-solve(data1,...)
  x$setInv(m)
  m
}
