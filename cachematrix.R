## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create a matrix with cache
makeCacheMatrix <- function(x = matrix()) {
  # initial the inverse matrix is not computed
  i_matrix <- NULL
  #set a new value for the matrix
  set <- function(y) {
    x <<- y
    #the inverse matrix is reset, because
    #is not valid anymore for the new matrix value
    i_matrix <<- NULL
  }
  get <- function() x
  #set a new value for the inverse of the matrix
  setinverse <- function(inverse_matrix) i_matrix <<- inverse_matrix
  getinverse <- function() i_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  # if it the inverse is already computed
  # it returnsit
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  # otherwise we compute the inverse of the matrix
  # with the solve() function and set the value
  # in the cache and returs the value
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
