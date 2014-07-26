## These two functions take a matrix input and cache the matrix and the matrix's inverse

## This function creates get and set functions for a matrix and for the inverse of that matrix, 
## assuming that the matrix is invertable.


makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y)  {

		x <<- y
		inv <<- NULL
	}

	get <- function() x
	
	setsolve <- function(solve) inv <<- solve
	getsolve <- function() inv
	
	list(get = get, set = set, 
		setsolve = setsolve, 
		getsolve = getsolve)

}


## A function that computes the inverse of the cached matrix if the inverse has not
## already been cached, and returns the inverse if it has been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getsolve()
	if(!is.null(inv)) {
		message ("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setsolve(inv)
	inv
}