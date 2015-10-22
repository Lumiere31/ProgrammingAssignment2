## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix which is a list containing functions :
## set() - to set the value of the matrix and its inverse
## get() - to get the value of the matrix
## setInverse() - to set the value of inverse of the matrix
## getInverse() - to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
		inverseMatrix <- NULL
		
		#set the value of the matrix and its inverse
		set <- function(y){
			x <<- y
			inverseMatrix <<- NULL
		}
		
		#get the value of the matrix
		get <- function() x
    
		#set the value of inverse of the matrix
		setInverse <- function(inverse) inverseMatrix <<- inverse
		
		#get the value of the inverse of the matrix
		getInverse <- function() inverseMatrix
		
		#return a list of all the functions
		list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve() calculates the inverse of the matrix
## First, a check is performed to see whether the inverse has already been calculated or not
## if so, it returns the inverse from the cache and skips the following computations
## else it calculates the inverse of the matrix and sets the value of inverse for cache via setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		##get the value of inverse
		inverse <- x$getInverse()
		
		##check if inverse is already calculated
		if(!is.null(inverse)){
			message("getting cached data")
			##return the cached value
			return (inverse)
		}
		
		##get the matrix data on which the inverse is to be calculated
		data <- x$get()
		
		##calculate the inverse
		inverse <- solve(data, ...)
		
		##set the inverse for cache
		x$setInverse(inverse)
		
		##return the inverse value
		inverse
}
