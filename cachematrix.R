## These functions will return the inverse of a matrix using cached data


## Function that creates a special matrix object that can cache the value 
## of its inverse

makeCacheMatrix <- function(x = matrix()) {

	#Function that creates a special matrix object that can cache its inverse
	
	#Create placeholder for our inverse result
	inv <- NULL

	#Function to rename object and reset inverse to NULL if required
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	#The get function which returns the original value of x
	get <- function() x

	#The setinv function which stores the inverse value for caching
	setinv <- function(inverse) inv <<- inverse

	#The getinv function which retrieves the value of inv
	getinv <- function() inv

	#List of functions that can be called to help a calling function access them
	list(get = get, 
		setinv = setinv,
		getinv = getinv) 

}


## Function which calculates the inverse of a matrix and can access cached values


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

	#Function which calculates the inverse of a matrix, x,  and can access cached values

	#Retrieve the value of the inverse
	inv <- x$getinv()

	#Conditions if the inv value is not NULL
	if (!is.null(inv)) {
		message("getting cached data")
		return inv
	}

	#If the inv value is NULL then retrieve the original value of x
	data <- x$get()

	#Calculate the inverse of the original matrix
	inv <- solve(data, ...)

	#Store the inverse value into inv so it can be retrieved.
	x$setinv(inv)
	
	#return the inverse
	inv

}
