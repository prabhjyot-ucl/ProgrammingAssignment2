## Program to find the inverse of a matrix using cached memory

##function makeCacheMatrix will  matrix store the inverse of a matrix in cache memory
makeCacheMatrix<-function(x=matrix()){
  m1 <- NULL
  set <- function(y) {
    x <<- y                   
    m1 <<- NULL   
  }
  get <- function() x
  setinv <- function(solve) m1 <<- solve
  getinv <- function() m1
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  ##solve(x)
}

##The function below will first look for array in cache. if it finds, it will directly return 
## the inverse from getinv() function in  'mackeCacheMatrix' otherwise it will compute the 
## inverse of the matrix using 'solve()' function.
cacheSolve <- function(x, ...) {
  m1 <- x$getinv()   ##Looks for m1 in cache
  
  if(!is.null(m1)) {                ##If m1 is not null then it directly gets returned
    message("getting cached data")  
    return(m1)        
  }
  
  ##if it does not find (i.e. if m1==null), the following lines will run,
  ## the solve() function is used to calculate inverse of the matrix
  data <- x$get() ##arguments
  m1 <- solve(data, ...)
  x$setinv(m1)
  m1
  
}