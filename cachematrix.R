## These functions inverse a matrix, inversion is performed only if it has not done before for a given matrix
## otherwise the inversed matrix is retrieved from cache

## makeCacheMatrix function build a vector with functions to calculate, store and retrieve the inverse
## of a matrix passed as its argument
makeCacheMatrix <- function(x = matrix()) {
  InversedMatrix <- NULL
  set <- function(y){
    x <<- y
    InversedMatrix <<- NULL
  }
  get <- function() x
  setInversedMatrix <- function(inverse) InversedMatrix <<- inverse
  getInv <- function() InversedMatrix
  list(set = set, get = get, 
       setInversedMatrix = setInversedMatrix, 
       getInv = getInv)
}

## cacheSolve calculates the inverse of the matrix store in the vector created in makeChaceMatrix 
## if the matrix is the same and it has been calculated then the inverse is retrieved from cache
## otherwise this function calculates the inversed matrix, and stores it in chache

cacheSolve <- function(x, ...) {
  InverseFunc <- x$getInv()
  if(!is.null(InverseFunc)){
      message("getting cached data")
      return(InverseFunc)
  }
  data <- x$get()
  InverseFunc <- solve(data)
  x$setInversedMatrix(InverseFunc)
  InverseFunc
}