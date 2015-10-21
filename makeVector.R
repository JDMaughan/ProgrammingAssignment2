makeVector <- function(x = numeric()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	setmean <- function(mean) m <<- mean
	getmean <- function() m
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x,...) {
	m <- x$getmean()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	} ##if
	
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}

crazy <- function() {
	x <<- 3.14 ## Set x in global environment
	print(x)
	print(x);
	x <- 42; ## Set x in local environment aka this function
	print(x)
	print(x)
}