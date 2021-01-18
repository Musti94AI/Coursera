#The following functions cache the inverse of a matrix


# Creates a special matrix object that can cache the inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  # Initialize inverse 
  i <- NULL
  
  # Set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ##  get matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  # set inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  #  get the inverse of matrix
  getInverse <- function() {
    # Return inverse 
    i
  }
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Set inverse 
  x$setInverse(m)
  
  # Return the matrix
  m
}
