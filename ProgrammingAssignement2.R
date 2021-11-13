makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL
  set <- function(y) { #set value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(mean) inv <<- inv  
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) { #compute the inverse oh a matrix 
  inv <- x$getinverse()  #return inverse of matrix x
  if(!is.null(inv)) { #retreave matrix from cashe if it is already existing
    message("getting cached data") #if matrix is retreaved from cach the message chows up
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
