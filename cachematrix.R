## This file has two functions for Programming Assignment 2.
## makeCacheMatrix() and cacheSolve()
## makeCacheMatrix() creates a list of functions that can be carried out on a matrix:
## 	set: define the matrix
## 	get: retrieve the matrix
## 	setinverse: stores the inverse of matrix
## 	getinverse: retrieves the inverted matrix
## 

## Write a short comment describing this function
## Create a List of set, get, setinverse, and getinverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	} ## set()
	
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse<- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	
	## Test if the inverted matrix has been created and cached, if so return it
	if(!is.null(m)){
		message("getting cached matrix")
		return(m)
	} ##if
	
	## If inverted matrix hasn't been created and cached, create it, cache it, and return it.
	mdata <- x$get()
	m <- solve(mdata,...)
	x$setinverse(m)
	m
}
