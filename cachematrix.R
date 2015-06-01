makeCacheMatrix <- function(x = matrix()) {
#Setting inverse as null
  i <- NULL
 #Set Function to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
 #Get Fucntion to get the matrix
  get <- function() x
  #Setting Inverse Matrix
  setInverse <- function(inv) i <<- inv
  #Getting Inverse Matrix
  getInverse <- function() i
  #Returning a list containing all the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
#Reading the inverse matrix
  i <- x$getInverse()
 #Checking if null or not . Returning Inverse if not null
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #If null , getting the original matrix
  data <- x$get()
  #Sovling to get inverse
  i <- solve(data, ...)
  #Setting Inverse Matrix
  x$setInverse(i)
  #Returning Inverse Matrix
  i
}
