##  Function that creates a special "matrix" object that can cache 
##  its inverse


makeCacheMatrix <- function(x = matrix()) {
  # getter and setter of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  # getter and setter of the inverse
  i <- NULL
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##  Function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # if inverse was already computed, then it just returns its value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # else it is computed and returned
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
