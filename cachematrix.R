## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special reprezentation of a matrix (a list of functions
## that set and get the matrix or inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix){
    x <<- matrix
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse){
    inv <<- inverse
  }
  getinverse <- function(){inv}
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

## Function that checks if the inverse was already calculated
## (if it was, dont repeat operation)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  # https://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
  im <- solve(data) %*% data
  x$setinverse(im)
  im
}
