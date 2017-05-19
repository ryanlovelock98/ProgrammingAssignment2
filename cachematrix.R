## Description
## The code contains two functions to return the inverse of the matrix. 
## The first allows the matrix to be inverted and the second checks to see whether the inverse exists

## The makeCacheMatrix function creates the list of functions needed for cacheSolve and stores the matrix to be inverted
## it will also clear the cache for the second folder

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  get <- function() x
 
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
## cacheSolve retruns the inverted matrix
##if the inverse matrix is not the cache, this function will create it and store it there.



cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
  
  calculatedInverse <- x$getInverse() 
  
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("cached data")
    return(calculatedInverse)
  }
  
  matrixToSolve <- x$get()  
  
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("not cached")
    message(w)
  }, error=function(e) {
    message("Matrix not solved")
    message(e)
    message("\n")
  })
  
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}

## if this is run twice, the message 'cached data' will appear
