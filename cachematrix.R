## Put comments here that give an overall description of what your
## functions do
      ##The pair of functions below work to store the inverse value of a matrix in cache, checking to see if the inverse is in cache first 
      ##in order to save time doing system taxing repetitive iterations of matrix inverse calcuation

## Write a short comment describing this function
        ## makeCacheMatrix is a function that stores a list of four functions that work together  
          ## set is a function that changes the matrix stored in the main function
          ## get is a function that returns the matrix x stored in the main function
          ## setinverse store the value of the input in a variable m into the main function
          ## getinverse gets the value of the input into m
        

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


## Write a short comment describing this function
        ## cacheSolve is a function that calculates the invers of a matrix if it is not already stored in cache
          ## verify the value m, stored previously with getinverse, exists and is not NULL
          ## if the value of m is null then it calculates the inverse of the matrix and x$setinverse(m) stores it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

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
