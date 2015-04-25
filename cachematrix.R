#A function that creates a special matrix which can cache it's inverse and
#another one for returning the inverse of the special matrix.


#creates matrix with cache for it's inverse
makeCacheMatrix <- function(x = matrix())
{
	#inverse not yet calculated, set initial value to NULL
	inverse <- NULL

	#function to change value of matrix
	set <- function(y)
	{
		x <<-y
		inverse <<- NULL  #erase previously calculated inverse
	}

	#function to return value of matrix
	get <- function() x

	#function to change cached inverse of matrix
	setInverse <- function(inv) inverse <<- inv

	#funtion to return inverted matrix
	getInverse <- function() inverse

	#return all functions so they can be accessed
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


#returns inverse of given matrix and stores it in cache if not already there
cacheSolve <- function(x,...)
{
	#if inverse is in cache return cached value
	inverse <- x$getInverse()
	if(!is.null(inverse)) return(inverse)

	#if not, calculate inverse, store it in cache and return it
	data <- x$get()
	inverse <- solve(data,...)
	x$setInverse(inverse)
	inverse
}