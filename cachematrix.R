## First function will return a vector which is actually a list which contains 4
## different functions and second function calculate the inverse of matrix,
## If the inverse is already stored in cache, it will skip the computation and
## returns the inverse which is stored otherwise it will calculate the inverse 
## and set in the cache


## This function returns a vector which is actually a list of 4 different 
## functions to set the matrix, to get the matrix, to set the inverse of matrix 
## and to get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(m)
  {
    x<<-m
    i<-NULL
    
  }
  
  get<-function() x
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## This function is used to calculate the inverse of matrix, if the inverse is 
## already stored, it will skip the computation and return the value stored in 
## cache else it will calculate the inverse using solve() function and set it 
## in cache for future use

cacheSolve <- function(x) {
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
  #returns a matrix which is the inverse of x
}
