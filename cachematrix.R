## Stores inverse matrices in cache and calls on them if the original matrix
## is recognized

## Computes the inverse of a matrix and stores the original and inverse
## in cache

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Searches cache first for the inverse of a given matrix; otherwise computes
## the inverse of the matrix

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix)
  x$setinverse(m)
  m
}
