## Assignment 2.Lexical Scoping
## Cache Matrix

makeCacheMatrix <- function(M = matrix()) {
  
  ##inverse matrix
  iM <- NULL
  
  ##set function for matrix
  set <- function( matrix ) {
    M <<- matrix
    iM <<- NULL
  
  ##get function for matrix  
  get <- function() {
    M
  }
  
  ##set inverse matrix 
  setInvM <- function(inv) 
    {
    iM <<- inv
  }
  
  ##get inverse of matrix
  getInvM <- function() 
    {
    iM
  }
  
  ##list with all aboved functions
  list(set = set, 
       get = get,
       setInvM = setInvM,
       getInvM = getInvM)
}


## Cache Solve

cacheSolve <- function(x, ...) {
  
  ##return inverse (x) matrix 
  M <- x$getInvM()

  ##check if the inverse has already been calculated
  if( !is.null(M) ) {
    print('getting cached data')
    return(M)
  }

  ##else inverse matrix is 0 or change, calculate inverse 
  data <- x$get()

  ##multiply matrix and solve inverse matrix
  M <- solve(data) %*% data

  ##set inverse to object
  x$setInvM(M)
  
  ##finally return matrix
  M
}

