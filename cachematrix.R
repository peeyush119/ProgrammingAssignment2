## These set of functions are enabling caching of inverse of a matrix
## If cache consists of inverse of a matrix, function returns from cache else, calculate the inverse and store in
## cache for future use.

## Defining a vector which enables storage of inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
    inversemat <- NULL
    set <- function(y) {
      x <<- y
      inversemat <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) inversemat <<- inversematrix
    getinversematrix <- function() inversemat
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)

}


## Function which returns inverse of a makeCacheMatrix vector if present in cache
## else, calculate and store it in the cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversemat <- x$getinversematrix()
  if(!is.null(inversemat)) {
    message("getting cached data")
    return(inversemat)
  }
  data <- x$get()
  inversemat <- solve(data)
  x$setinversematrix(inversemat)
  inversemat
}
