# This function creates a matrix structure. cacheSove then creates the inverse of # the matrix created.  

makeCacheMatrix <- function(x=matrix()) {
xinv <- NULL
set <- function(y)
{
x <<- y
xinv <<- NULL
}
get <- function() x
setinverse <- function(inverse) xinv <<- inverse
getinverse <- function() xinv
list(set = set, get = get,
      getinverse = getinverse,
      setinverse = setinverse)
}

# This function creates inverse of a matrix created in makeCacheMatrix function. 
# If the inverse is present already, it is retrieved. 

cacheSolve <- function (x, ...) {
xinv <- x$getinverse()
  if (!is.null(xinv)) {
  message("Cached inverse matrix")
  return(xinv)
  }
  else 
  {
  xinv <- solve(x$get())
  x$setinverse(xinv)
  return(xinv)
  }
}
