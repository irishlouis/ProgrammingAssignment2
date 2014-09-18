## cacheSolve takes in a array 'x' as an argument and returns inv x
## the inverse is stored in makeCacheMatrix using getters and setters

## makeCacheMatrix takes in a matrix and creates a cache matrix for storing inverse
## this function must be called first
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes in array x and if inverse already exists in cache returns w/o
## recalculating, else the inverse is calculated using solve() and set into cache
## using $setinverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

