## makeCacheMatrix creates a list, containing a function to
##	1) set the value of the matrix
##  2) get the value of the matrix
##  3) set the value of the inverted matrix (inv)
##  4) get the value of the inverted matrix (inv)
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


## cacheSolve returns you the cached inverted matrix if it exists and if the original matrix hasn't changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv) & identical(x$get, solve(inv))) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
