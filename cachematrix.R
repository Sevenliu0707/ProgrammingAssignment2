## Put comments here that give an overall description of what your
## functions do



makeCacheMatrix <-function(x=matrix()){
    ##initializing inverse 
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##get matrix x
  get <- function(){x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}

##This function is used to get the cache data
cacheSolve <-function(x,...){
  inv <-x$getInverse()
  ##check inverse, if NULL, return inverse value
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data <-x$get()
  inv<- solve(data,...)
  x$setInverse(inv)
  inv
}
