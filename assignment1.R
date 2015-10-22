
## MakeCacheMatrix is a function,
## it creates a This function creates a special "matrix" object that can cache its inverse.

MakeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## CacheSolve is a function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting the cached data")
    return(m)
  }
  matrix <- x$get() 
  
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}