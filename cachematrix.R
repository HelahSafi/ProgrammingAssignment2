## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  J<-Null              #initializing J as null
  set<-function(y){
    x<<-y
    J<<-NULL
  }
  get<-function()x          #function to get matrix x
  setinverse<-function(inverse) J<<-inverse
  getinvverse<-function() J 
  list(set= set, get = get, setinverse= setinverse, getinverse= getinverse)
}


## Write a short comment describing this function
##this is used to get the cache data

cacheSolve <- function(x, ...) {
  
  J<-x$getinverse()
  if (!is.null(J)) {         #checking whether J is null
    message ("getting cached data!")
    return(J)               #returns J value
  }
  data<-x$get()
  J<-solve(data,...)      #calculate J value
  x$setinverse(J)
  J
        ## Return a matrix that is the inverse of 'x'
}
