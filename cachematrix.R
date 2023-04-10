## These functions create a matrix and assign variables to its data to be called, then uses these variables to determine if the matrix has been inversed already, and if not will inverse the matrix.

## This function creates the Cache Matrix and assigns variables for the data and subsequent variables. X = data/the matrix; m = the inverse matrix; y = a copy of the orginal matrix.  
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}  

## This function calls the inverse of a matrix. If the matrix has been inversed before, it will pull that data from the cache. If not, the function will create the inverse then add it to the cache.

cacheSolve <- function(x, ...){
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- solve(data, ...)
  x$setsolve(m)
  m
}
