makeCacheMatrix <-function(x=matrix()){
 
   i<-NULL
  set<-function(y){
    
    x<<-y
    i<<-NULL
    
  }
  
  get<-function() x
  
  set_inv <- function(inv) i<<-inv
  get_inv <- function() i
  
  list(set=set,get=get,set_inv = set_inv, get_inv = get_inv)
}



cacheSolve <- function(x, ...){
 
   i<- x$get_inv()
  if(!is.null(i)){
    
    message("Getting Cached Data...")
    return(i)
    
  }
  
   data<-x$get()
   i<-solve(data, ...)
   x$set_inv(i)
   i
   
}