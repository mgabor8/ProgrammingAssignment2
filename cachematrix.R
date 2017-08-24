## makeMatrix creates a list of functions that
## cache the matrix we want to inverse and
## can be extracted by the cacheSolve function.

## The first element in the list, $set, is a function that
## changes the value of the object created by makeMatrix without initializing another instance of the object.

## The second element in the list, $get, is a function that stores the matrix we want to inverse
## calling x$get() returns the matrix inputted into makeMatrix

## The third element in the list, $setinverse, is used when it is extracted by the cacheSolve function
## It stores the inverse of the matrix (using the solve() function) in inv.

## The fourth element in the list, $getinverse, returns the value of inv 
## inv will be the inverse of the matrix if it has been cached
## or NULL if it hasn't.

makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve first checks to see if an inverse of the matrix has been stored
## and if not, calculates the inverse of the matrix and stores it in the cache.

cacheSolve <- function(x, ...) {
    ## cacheSolve first checks if the inverse of the matrix has been cached
    ## If it has, cacheSolve shows the message "getting cached data" and returns the inverse of the matrix.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    ## If it hasn't, cacheSolve extracts the matrix from the list created by makeMatrix,
    ## inverts it
    ## stores the inverse in the cache.
    ## then returns the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}