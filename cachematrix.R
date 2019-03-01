##Functions: makeCacheMatrix to calculate and store the inverse of a matrix.
## cacheSolve to retrieve the cached data, if available and return it without further computations. 
##If unavailable, will use the functions in makeCacheMatrix to compute and return the inverse. 


## makeCacheMatrix takes a matrix as an argument, gets the inverse of said matrix and stores it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## CacheSolve verifies if the inverse of the matrix has been calculated by makeCacheMatrix. 
## if it has, it will return the cached inverse without any further calculation. 
## if it hasn't it will run the necessary functions in makeCacheMatrix in order to get, set and return the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
