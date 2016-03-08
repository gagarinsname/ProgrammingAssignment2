## makeCacheMatrix creates a list, containing a function to
##	1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the value of the inverted matrix (inv)
##  4) get the value of the inverted matrix (inv)
## functions do
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(invert) inv <<- invert
	getinv <- function() inv
	list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  data <- matrix()
	inv <- x$getinv()
	if(!is.null(inv)) {
    sum(x$get()==solve(x$getinv())) == sum(dim(x$get))
    if(sum(x$get()==solve(x$getinv())) == dim(x$get())[1] * dim(x$get())[2]){
		  message("getting cached data")
		  return(inv)
	  }
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
