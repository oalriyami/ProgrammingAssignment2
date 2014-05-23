## This function is used to create a speacial matrix
## with caching capabilities for its inverse. The function
## creates the speacial matrix with 4 additional functions:
##    1. set:    sets the value of the speacial matrix to the 
##               passed matrix and sets the value of the inverese to NULL
##    2. get:    get the original matrix
##    3. setinv: set the value of the inverse to the passed value
##    4. getinv: get the inverse of the speacial matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,get = get,
       setinv = setinv,
       getinv = getinv)
}


## Compute the inverse of the speacial matrix created from makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  ## Get the cached inverese and check if it already exists
  ## Return the inverse if it exists (not NULL)
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## Compute the inverese and set it in the speacial matrix
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  
  ## Return the inverse
  inv
}
