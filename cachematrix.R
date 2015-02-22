## The following functions will create an inverse of a matrix using the solve function,
## the result of the solve function will be cached so that it can be used again.
## The functions below will determine if the inverse of the matrix has been previously calculated,
## if the inverse has been previously calculated then the cached result will be returned,
## else the inverse will be calculated,stored into the cache and returned.

## makeCacheMatrix creates all the functions required to manage the storing of a matrix 
## and cache the results of the solve function
## This function will store the assigned matrix for use later if regarded.

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<- function(y){
		x <<- y
		m <<- NULL
	
	}
	get <- function() x
	setMatrix <- function(solve) m <<- solve
	getMatrix <- function() m
	list(set = set, get = get,
		setMatrix = setMatrix,
		getMatrix = getMatrix)

}


## cacheSolve will return an inverse of a matrix, the result will be returned from the cache if available 
## or calculated using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getMatrix()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setMatrix(m)
		m
}
