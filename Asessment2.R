makeCacheMatrix <- function(x = numeric()) {
  s <- NULL  
  set <- function(y) {
    x <<- y  
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cachesolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <-solve(data) 
  x$setinverse(s)
  s
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2) 
test<-makeCacheMatrix(m1)
test
cachesolve(test)