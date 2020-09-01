##This function calcuates a matrix and looks for the inverse of it

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL 
  set=function(y){
    x<<- y
    inv<<- NULL
  }
  get=function(){x}
  setinverse=function(inverse){inv<<-inverse}
  getinverse=function(){inv}
  list(set= set, get=get, setinverse= setinverse, getinverse= getinverse)
}


##This function looks for the inverse of the matrix in the environment
#and if it wasn´t calculated yet, it calculates it 

cacheSolve <- function(x, ...) {
  inv=x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat= x$get()
  inv=solve(mat,...)
  x$setinverse(inv)
  inv
        
}
