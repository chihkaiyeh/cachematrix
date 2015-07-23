##Includes 2 parts, "makeCacheMatrix()" and "cacheSolve()", which can work together 
##to create matrix and to calculate the inverse matrix.

## makeCacheMatrix()
## Includes 4 functions:
## 1. set(): set value of matrix
## 2. get(): get value of matrix
## 3. setI(): set value of inverse matrix
## 4. getI(): get value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL

  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x

  setI <- function(inverse) s <<- inverse
  getI <- function() s
  
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}

## cacheSolve()
## Compute the inverse of the matrix which returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getI()
  
  if (!is.null(s)) {
    message("getting cached data")
    
    return(s)
  }
  
  m <- x$get()
  
  s = solve(m, ...)
  
  x$setI(s)
  
  return (s)
}
