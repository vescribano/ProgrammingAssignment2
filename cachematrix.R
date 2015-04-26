## makeCacheMatrix creates a special "matrix" object that can cache its inverse.


## mat is the matrix to be inversed
## returns a vector of functions: setMatrix, getMatrix, setInv and getInv
## procedure: assigns NULL to invMatrix and defines the functions to be returned

makeCacheMatrix <- function(mat = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    mat <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() mat
  setInv <- function(inv) invMatrix <<- inv
  getInv <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

## cCache: is the result of applying makeCacheMatrix to the original matrix
## newMatrix: new matrix. Default: matrix
## returns invMatrix. If it is cached, it prints "getting cached data"
## procedure: if new matrix is given and is not equal to the default matrix
## AND new matrix is not identical to the cached one,
## new matrix substitutes the cached one.
## Then, if cached invMatrix is not NULL, we get invMatrix and it prints out
## "getting cached data". If invMatrix is NULL it applies solve function
## to the cached matrix, the result is stocked as a new cached invMatrix and
## invMatrix is returned.


cacheSolve <- function(cCache,newMatrix=matrix(), ...) {
  if (!identical(newMatrix,matrix())) {
    if(!identical(newMatrix,cCache$getMatrix())) cCache$setMatrix(newMatrix)  
  }
  invMatrix <- cCache$getInv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- cCache$getMatrix()
  invMatrix <- solve(data, ...)
  cCache$setInv(invMatrix)
  invMatrix
}
