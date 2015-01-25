## This program ensures that a calculation (inverse) on a matrix is not repeated. 
## This is achieved by storing the inverse in a Cache and checking for its existense. 
## If the inverse is found a message says so and skips the recalculation.

## This function is an inventory or repository of sub-functions that store a matrix and its inverse to cache.
## 2 sub-functions are created to calculate or access the cached inverse 
## This function outputs these usable sub-functions to a list.

makeCacheMatrix <- function(x = matrix()) {     # decalares that input is a matrix
  m <- NULL                   # m is the cache variable, gets initialized to null for a new calc
  set <- function(y) {        # this function stores the new matrix to a free var x, and nulls m
    x <<- y
    m <<- NULL
  }
  get <- function() x         # create a function that fetches stored matrix
  setinverse <- function(solve) m <<- solve # create a function that calculates matrix inverse
  getinverse <- function() m  # create a function that fetches stored inverse
  list (set = set, get = get, # output functions to a list
        setinverse = setinverse, getinverse = getinverse)
}

## This function checks if a cached value for the input matrix exists. If not it calls functions
## to calculate the inverse. 

cacheSolve <- function(x, ...) {          # the argument is the list or ouptut of makeCacheMatrix 
    m <- x$getinverse()                   # retreive the value(inverse) stored in cache     
    if(!is.null(m)){                      # check whether m has a cached value or is null
        message("getting cached data")
        return (m)                        # if cache exists, return the storage m
    }
    data <- x$get()                       # if cache was null, read the new matrix
    m <- solve(data, ...)                 # calculate inverse of new matrix
    x$setinverse(m)                       # store the inverse in a cache
    m                                     # output the inverse
}