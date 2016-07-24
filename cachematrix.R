## The makeCacheMatrix function creates a special matrix which can cache its inverse
## The cacheSolve function retrieves a matrix from the cache if it is found
## If the matrix is not found, cacheSolve calculates the inverse and returns it.

## This function allows caching of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
	
	# Set matrix
        set <- function(y) {
                x <<- y

		mi <<- NULL
	}

	# Get original matrix
	get <- function() x

	# Save inverse of matrix in the cache
	setinv <- function(inv) mi<<- inv

	# Get inverse of matrix from the cache
	getinv <- function() mi
	list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	# Return the inverse of a matrix using getinv() function of makeCacheMatrix   
	m <- x$getinv()

	# Check if the inverse was found. If found then return from cache
	if(!is.null(m)){
		message("Getting cached data")
		return (m)
	} 

	# If it was not found then, calculate the inverse and save it in the cache
	data <- x$get()
	m <- solve(data)
	x$setinv(m)
	m
}
