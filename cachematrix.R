## cachematrix.R - Assignment for R-Programming (Week 3)
## csun008 (Changmin Sun)
## 1 Nov 2020. 
##
## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix function creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache


makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  set <- function(y) {
    x <<- y    # Set the value
    cache <<- NULL # Clear the cache
  }
  # Define function to get the value of the matrix
  get <- function() x
  # Define function to invert the matrix and store in cache 
  setInverse <- function(inverse) cache <<- inverse
  # Define function to get the inverted matrix from cache
  getInverse <- function() cache
  
  # Return the created functions to the working environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,it is created
## in the working environment and it's inverted value is stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache<- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if(!is.null(cache)){
    message("getting cached matrix")
    
    #display matrix in console
    return(cache)
  }
  
  # create matri since it doesn't eist 
  data<- x$get()
  
  # set and return inverse of matrix
  
  cache <- solve(data,...)
  
  x$setInverse(cache)
  
  #display inverted matrix in console
  return(cache)
}


## Sample run-time example included results
## > source("cachematrix.R")    load R program
## > m1<- matrix(c(1/2, -1/4, -1, 3/4),nrow=2,ncol=2) 
##                                      test data provided by course mentor Alan
## >m1
##       [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## > myTestMatrix <- makeCacheMatrix(m1)     create matrix in working environment
## > cacheSolve(myTestMatrix)   1st run returns inverted matrix
##                              from working environment
##        [,1] [,2]
## [1,]    6    8
## [2,]    2    4
##
## > cacheSolve(myTestMatrix)   2nd and subsequent runs
##                              returns inverted matrix from cache
## getting cached data          
##        [,1] [,2]
## [1,]    6    8
## [2,]    2    4
