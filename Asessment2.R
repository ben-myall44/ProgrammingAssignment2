
#creates matrix object
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL  

 #method to set matrix 
  set <- function(y) {
    x <<- y  
    s <<- NULL
 
  }
  
  #Gets the Matrix
  
  get <- function() x
  #Sets the Inverse 
  
  setinverse <- function(solve) s <<- solve
  
  #Gets the Inverse
  getinverse <- function() s
  
  #Retaurns a list of elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cachesolve <- function(x, ...) {
  #returns inverse of matrix x
  s <- x$getinverse()
 
  #returns inverse if already set 
   if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #gets matrix from the object
  data <- x$get()
  #calculates inverse
  s <-solve(data)
  #sets inverse to object
  x$setinverse(s)
  #returns matrix
  s
}

#Eample to show the function works
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) 
test<-makeCacheMatrix(m1)
test
cachesolve(test)