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
