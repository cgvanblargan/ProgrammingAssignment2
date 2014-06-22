## The functions makeCacheMatrix and cacheSolve are used together to calculate inverses
## of matrices and store them so that any time a matrix is inputted, the function
## first checks to see if the inverse has prevously been computed and uses that result


## makeCacheMatrix takes in a matrix, will get and set set the value of the matrix,
#and will get and set the value of the matrix's inverse

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


## Cachesolve takes in list from makeCacheMatrix, will return the inverse of the 
##matrix, it will first check to see if the matrix's inverse has previous been cached
## and so will not recompute inverse, if the matrix has not been that it will
## compute the inverse and then sets the inverse using function from makeCacheMatrix

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
