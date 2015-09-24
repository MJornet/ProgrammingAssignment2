## Calculate the inverse of a matrix
## cache the answer, in order not to do the same calculation twice


## Create the cache input and output matrices
makeCacheMatrix <- function(inputMatrix = matrix()) {
  #the result is initialized
  inverseMatrix <- NULL
  #Adding it to the cache
  set <- function(y) {
    inputMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() inputMatrix
  setsolve <- function(solve) inverseMatrix <<- solve
  getsolve <- function() inverseMatrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#check if the matrix is cached to return the cached inverse, otherwise calculate the inverse and cache it
cachesolve <- function(inputMatrix, ...) {
  #If the matrix is cached, we already have the solution
  inverseMatrix <- inputMatrix$getsolve()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  #If it is not cached, we calculate it
  data <- inputMatrix$get()
  inverseMatrix <- solve(data, ...)
  inputMatrix$setsolve(inverseMatrix)
  inverseMatrix
}
