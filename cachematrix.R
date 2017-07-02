## A set of functions to cache the inversion of a matrix and retrieve it if needed.

## This function creates a list of functions that sets up the next set.

makeCacheMatrix <- function(x = matrix()) {   ##define fxn and be prepared to coerce x to matrix
  cacheInv <- NULL                            ##initialize cacheInv, which will hold the stored value
  set <- function(y) {                        ##initialize set fxn with value of y (which will be passed when x$set is called)
    x <<- y                                   ##set x (in parent environment) to value of y
    cacheInv <<- NULL                         ##set cacheInv (in parent env) to NULL after setting x
  }
  get <- function() x                         ##used for when runnin x$get - will give value of x stored in current env
  setSolve <- function(solve) cacheInv <<- solve  ##defines setSolve as a function setting cacheInv in the parent env to the calculated value
  getSolve <- function() cacheInv             ##function used to get stored value when calling x$getSolve
  list(set=set, get = get, setSolve = setSolve, getSolve = getSolve)  ##creates vector of functions to be used in cachematrix
}


## This function returns a matrix that's the inverse of 'x' - caclulated if not
## already cached

cacheSolve <- function(x, ...) {              ##defines cacheSolve function with input of x and defaults
        ## Return a matrix that is the inverse of 'x'
  cacheInv <- x$getSolve()                    ##if present, this will pull whatever is in cachceInv from the parent env using x$getSolve
  if(!is.null(cacheInv)) {                    ##if cacheInv has a value, send msg and return cached value        
    message("getting cached data")
    return(cacheInv)
  }
  data <- x$get()                             ##assuming cacheInv does not have a value, get the matrix data to calculate it
  cacheInv <- solve(data, ...)                ##perform the laborious and time-consuming matrix inversion calculation
  x$setSolve(cacheInv)                        ##use x$setSolve to store the result in the parent env so you don't have to calculate it again
  cacheInv                                    ##don't forget to return the result
}
