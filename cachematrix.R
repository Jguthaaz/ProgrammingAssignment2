# Coursera - Programming Exercise
# Jorge Gutierrez Haaz

## Description of the makeCacheMatrix:

## Assume we apply the command: a <- makeCacheMatrix() 
## then it creates an object named a and calls the makeCacheMatrix() function to instantiate it: 
##  When we call the function and don't reference any of the subfunctions, then it sets cachedInv to NULL (cache)
##  The result is that when we apply this command in a , then 
##  a is an object that contains an empty numeric vector, has an internal variable cachedInv which is set to NULL, 
##  and has four subfunctions that can be called to work on it (set(), get(), setInverse(), and getInverse())


makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  ## set x in parent environment with the desired value
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  ### The get() subfunction obtains the matrix that might be stored in x and returns it.
  ### There are two other subfunctions in makeCacheMatrix():
  ### The first is is setInverse().  It takes a matrix passed into it and stores it in cachedInv, the cache. 
  ### Similarly, getInverse() return the cache.
  
  get <- function() x
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Description of the function cacheSolve

## This calculates the inverse of the "matrix" created with the makeCacheMatrix function.
## First of all, it checks if the inverse has already been calculated; if so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x=makeCacheMatrix(1:9, nrow=3, ncol=3), ...) { ##special matrix provided or create a test 2x2 matrix
  
  ## Check if something is already there 
  calculatedInverse <- x$getInverse() 
  
  ### Check if the returned cache has anything in it. If so, we print the message and return the cached matrix 
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("Cached data already exists")
    return(calculatedInverse)
  }
  
  ### otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ### try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("It might not be the result...")
    message(w)
  }, error=function(e) {
    message("Something went wrong...")
    message(e)
    message("\n")
  })
  
  ### whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}



## RESULTS EXAMPLE:

### > M<-cbind( c(0.2, 0.9, 1), c(1.0, 5.1, 1), c(6, 0.2, 1)) 
### > a <- makeCacheMatrix(M)
### If we apply cacheSolve(a) for first time:
### Then we obtain the next results:
### "Setting the value of inverse to:"
###        [,1]        [,2]         [,3]
###        [1,] -0.19662921 -0.20064205  1.219903692
###        [2,]  0.02808989  0.23274478 -0.215088283
###        [3,]  0.16853933 -0.03210273 -0.004815409

### If we apply cacheSolve() for second time:
### Then message "Cached data already exists" appears...




