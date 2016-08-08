## Set of functions for solving the second assigment. Focus on clarity rather than code volume minimization.

## This function caches the inverted matrix, enables passing the input matrix and returns list of access functions, 

makeCacheMatrix <- function(x = matrix()) {
  
  #Flag to check if solution already exists + access and setting functions
  cacheExists<-FALSE 
  getFlag<-function() cacheExists
  setFlag<-function(value) cacheExists<<-value # <<- required because we want to update makeCacheMatrix from inside of cacheSolve
  
  #function to pass input argument
  getData<-function() x
  
  #variable for storage of the solution + access and setting functions
  cachedMatrix<-NULL
  getCache<-function() cachedMatrix
  setCache<-function(matr) cachedMatrix<<-matr # again, <<- needed becouse we seet this value out of scope of this function
  
  #returned list of access functions
  list(getFlag=getFlag,setFlag=setFlag, getCache=getCache, setCache=setCache, getData=getData)
}

## This function takes as input the output from the makeCacheMatrix() function. Checks if the inverted matrix has already been 
## calculated for given input, if yes returns cached result, if not calculates the inverted matrix, updates cache and sets the flag

cacheSolve <- function(x, ...) {
  
  if (x$getFlag()) 
  {
    print("taking cached matrix")
    x$getCache()
    return(x$getCache())
  }
  
  temp<-x$getData()
  result<-solve(temp)
  x$setCache(result)
  x$setFlag(TRUE)
  result
  return(result)
  
  ## Return a matrix that is the inverse of 'x'
}


