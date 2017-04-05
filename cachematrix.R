makeCacheMatrix <- function(x = matrix()) 
## Function Definition by passing Matrix as an arguement
{
  mtx <- NULL           ##Set Matrix to Null
  set <- function(y)    ##Set Function 
  {
    x <<- y            ##Assign Value Y to Object X
    mtx <<- NULL       ##Set Matrix to Null
  }
  get <- function() x ##Get Matrix x
  setinverse <- function(solve) mtx <<- solve   ##Set the inverse Matrix
  getinverse <- function() mtx                  ##Get the Inverse of matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)##Construct,Coerce and check for both kinds in list 
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) 
  {
    mtx <- x$getinverse()     ##Get inverse of matrix vector
    if(!is.null(mtx))         ##Checking for Not Null Matrix
      { 
      message("getting cached data")  ##If Not Null Display the message
      return(mtx)                     ##Return the Cached Data matrix
      }
    data <- x$get()                   ##Get matrix from x and assign it to Data
    mtx <- solve(data, ...)           ##Assining the solved matrix to mtx matrix
    x$setinverse(mtx)                 ##Inversion the the matrix
    ## Return a matrix that is the inverse of 'x'
    mtx
  }      
