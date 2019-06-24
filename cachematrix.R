## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix creates a special “matrix” object that can cache its inverse.
 ##This function creates a list object from the input matrix containing functions to
 ##1.set the value of the matrix,
 ##2. get the value of the matrix,
 ##3. set the value of the inverse,
 ##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverseMatrix) inv <<- inverseMatrix
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve fucntion computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.


cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        
        data <- x$get()
		
		##Computing the inverse of a square matrix can be done with the solve function in R.
        inv<- solve(data,...)
        x$setinv(inv)
        inv
}
