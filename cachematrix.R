## These functions provide a cacheing mechanism for matrix inversion.  
## A "cache-enabled matrix" is created by calling makeCacheMatrix(x)
## The inverse can be obtained by calling cacheSolve(x).
## If available, the cached inverse will be returned.

## This function makes a "matrix" with the capability of cacheing its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function () inv
	list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## This function returns the matrix inverse of x, from cache if possible

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
#		message("getting cached inverse")
		return(inv)
	}
 	data <- x$get()
	inv <- solve(data, ...)
      x$setinv(inv)
	inv     
}
