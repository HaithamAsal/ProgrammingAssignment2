## Put comments here that give an overall description of what your
## functions do
## the function is mainly for cashing the inverse of a matrix 
##so that it can be reutilized later and in different environment

## Write a short comment describing this function
## The makecacheMatrix function created a matrix object to be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse 
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## the cacheSolve function computes the inverse of matrix returned by the makecaheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}


## the below shows the testing of the functions and returning the inverse of the matrix
## > test_assignW3 <- matrix(rnorm(24),4,4)
## > Cache_assignW3 <- makechachematrix(test_assignW3)
## > cacheSolve(Cache_assignW3)
## [,1]      [,2]       [,3]      [,4]
## [1,]  23.61002  134.7905  -52.29518  3.548696
## [2,]  81.19670  465.6712 -181.41574 14.155849
## [3,]  43.32031  246.8002  -96.84957  7.094324
## [4,] -24.26790 -140.6408   54.95931 -4.110597
