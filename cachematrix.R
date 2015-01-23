## This R script takes a matrix for instance the 2x2 matrix
##
##  [ 1   , -0.25]
##  [-0,25,  1   ]
## 
## and calculates the inverse of the matrix. This is easily solved using the function solve in R
## but to save time if you have to calculate the same result again the result is cached inside the function
## and reused if called with the same data.
##
## Example use:
##
## a <- rbind(c(1,-0.25),c(-0.25,1)) ## This creates the matrix
##
## cachedMatrix <- makeCacheMatrix(a)  ## This creates the cachedMatrix, actually a list that will be used instead of the orginal matrix
##
## cacheSolve(cachedMatrix) ## this prroduces the result. First time it calculates it but next time it will use the cached value.
##
##
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 
##  set the values of the matrix
##  get the values of the matrix
##  set the values of the inverse matrix
##  get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x  <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inverse) m <<- inverse
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets 
## the value of the inverse matrix in the cache via the setinversematrix function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("Using cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
