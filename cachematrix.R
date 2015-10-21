## This file has two functions for Programming Assignment 2.
## makeCacheMatrix() and cacheSolve()
## makeCacheMatrix() creates a list of functions that can be carried out on a matrix:
## 	set: define the matrix, or set it's value
## 	get: retrieve the matrix
## 	setinverse: set the value of the inverse of the matrix
## 	getinverse: retrieves the value of the inverse of the matrix
## 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	} ## set()
	
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse<- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
} ## makeCacheMatrix()


## Write a short comment describing this function
## This function returns the inverse of a matrix. It first checks if the inverse has already been computed.
## If inverse is already computed, it gets the result and skips the computation.
## If inverse is not already computed, it computes the inverse, sets the value of the inverse vi setinverse() function.
## 
cacheSolve <- function(x, ...) {
        ## Get inverse value of the matrix
	inv <- x$getinverse()
	
	## Test if the inverted matrix has been created and cached, if so return it
	if(!is.null(inv)){
		message("getting cached matrix")
		return(inv)
	} ##if
	
	## If inverted matrix hasn't been created and cached, create it, cache it, and return it.
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
} ## cacheSolve()
