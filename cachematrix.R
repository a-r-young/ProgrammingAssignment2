## These functions are creating a special matrix that attempts to cache the matrix and it's inverse 
## at the same time. 

## This function is setting the variables for the matrix to be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) s <<- solve
      getinverse <- function() s
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## used to cache the inverse of a matrix

cacheSolve <- function(x, ...) {
      s <- x$getinverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data) %*% data
      x$setinverse(s)
      s
}
