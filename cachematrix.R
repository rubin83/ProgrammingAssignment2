## Takes a matrix(called inputMat) and calculates the inverse of that matrix. If the matrix already exist,
## calls the solution from the cache

## cache function that sets and retrieves the value of the inputed matrix, as well as, sets and retrieves the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# the function that retrieves the inverse if it exists. If doesn't, calculate and stores the inverse
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

inputMat <- matrix(c(1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 0, 1, 4, 2, 3), 4, 4)
cache <- makeCacheMatrix(inputMat)
cacheSolve(cache)

# repeat to verify getting cached data
cacheSolve(cache)
