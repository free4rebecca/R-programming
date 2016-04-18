## The functions are to evaluate if a matrix inversion has been computated.
## If it has been computated, the functions will cache the inverse of a matrix

## This function is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function is to compute the inverse of the matrix created by "makeCacheMatrix" function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message ("getting cache data")
    return (m)
  }
  data <-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
