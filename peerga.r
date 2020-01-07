makeCacheMatrix <- function(x = matrix()) {  #We define a matrix as a default value

  i <- NULL   #this value will hold the inverse matrix
  set <- function(y){     #we can asignd a new value
    x <<- y     #parent environment for the matrix
    i <<- NULL      #to reset to NULL
  }
  
  get <- function()x   #we define a new function that gets the matrix that we want
  setInverse <- function(inverse) j <<- inverse  #inverse value in parental environment
  getInverse <- function() j    #gets the value of the inverse
  list(set = set, get = get,  
  setInverse = setInverse, 
  getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
