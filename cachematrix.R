## 1st function set and get the value of the matrix x and its inverse matrix
## 2nd function calculates the inverse of the matrix x.
## But it first checks to wheather the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## If not, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.


## Creates an object storing a matrix and its inverse 
## (with getters and setters for the matrix itself)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## In case there is a new Matrix then get the new one and delete stored inverse cache  set <- function(y) {
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  ## get matrix x
  get <- function() x

  ## set the inverse of x  
  setinverse <- function(inverse) inv <<- inverse

  ##get the inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Returns the inverse of x (data should be set already). 
## In case inverse has already been computed, it uses cache, if the inverse 
## is not computed it computes it and stores it to cache prior to returning it 

cacheSolve <- function(x, ...) {
  
  ##Cache into inv
  inv <- x$getinverse()
  
  ##If the cache is empty then return a matrix which is inverse of matrix x...
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##...else get data for inverse computation, compute the inverse matrix and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return the inverse matrix of x
  inv
}
