## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and creates a special "matrix", 
## which is really a list containing functions to: 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	## m stores the cached inverse, initialized to NULL
	m <- NULL

	## set the value of the matrix
      set <- function(y) {
      	x <<- y
      	m <<- NULL
      }

	## get the value of the matrix
      get <- function() x

	## set the inverse of the matrix using the cached value "m"
      setinverse <- function(inverse) m <<- inverse

	## gets the inverse of the matrix for the cached value "m"
      getinverse <- function() m

	## returns a list containing the functions defined above.
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve takes a special "matrix" created by the makeCacheMatrix function
## cacheSolve returns the cached value of the inverse of a "matrix" 
## if the value was previously set using makeCacheMatrix$set.  If the  
## inverse is not set, then cacheSolve calculates the inverse of the matrix
## using the base function solve(x) 

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	## retrieve cached inverse value 
      m <- x$getinverse()

	## if results are not NULL, return the cached inverse
      if(!is.null(m)) {
            message("getting cached data")
      	return(m)
      }
	
	## retrieve the matrix
	data <- x$get()
      
	## solve the matrix
	m <- solve(data, ...)

	## cache the inverse
      x$setinverse(m)
 
	## return the inverse
	m
}

## wrote a test script to verify the functionality
test <- function(){
	x <- matrix(c(0,0,2,0,0,0,0,2,2,0,0,0,0,2,0,0),4,4)
	y <- solve(x)
	a <- makeCacheMatrix(x)
	cacheSolve(a)
	a$getinverse()
}
