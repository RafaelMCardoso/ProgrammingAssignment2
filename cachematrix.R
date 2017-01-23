## creates a special matrix that can store the inverse of a matrix
## that can be allocated in the cache and saves computation time;

makeCacheMatrix <- function(x = matrix()) {
  
  ## Creates a list where each element is a function that is may be called by the cacheSolve()
  
  s<-NULL
  set<-function(y){
    ##exists to replace the values of x;
    x<<-y
    s<-NULL
  }
  get<-function()
    x
  setsolve<-function(solve) s<<-solve
  getsolve<-function() s
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}




cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x';
  
  s <- x$getsolve()

    if(!is.null(s)) {
  ##if the solve was already cached, s isnt NULL and it can be returned as a cached data
    message("getting cached data")
    return(s)
    }
  ##if its new data thats been called, create a variable called data, wich includes the x values
  ##solve the data, set as "s", cache it to x$setsolve and print;
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
