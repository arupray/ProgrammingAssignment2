## Put comments here that give an overall description of what your
## functions do 
## --------------------------------------------------------------
## There are two main functions - one that creates a special kind
## of matrix that can allow the storage and retrieval of the 
## inverse.  The cacheSolve function when called either retrieves
## the inverse or calculates the inverse and stores it in the
## makeCacheMatrix object.
## --------------------------------------------------------------
## Write a short comment describing this function
## --------------------------------------------------------------
## makeCacheMatrix:  This function takes in a matrix and creates 
## a list of functions that provide the following functionalities:
## 1.  get returns the core matrix
## 2.  set allows us to modify the matrix and NULL out the inverse
## 3.  getInverse returns the inverse
## 4.  setInverse stores the inverse that is passed to it 
## --------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # initialize the inverse to NULL

    # the set function
    set <- function(y){
	x <<- y
	inv <<- NULL
    }

    # the get function
    get <- function() x

    # the getInverse function
    getInverse <- function() inv

    # the setInverse function
    setInverse <- function(inverse) inv <<- inverse

    # the return is a list of the function
    list(get=get, set=set, getInverse = getInverse,
		setInverse = setInverse)
}

## --------------------------------------------------------------------
## Write a short comment describing this function
## --------------------------------------------------------------------
## cacheSolve:  this function takes a special matrix created by the
## function makeCacheMatrix as its argument.  It then attempts to get
## the inverse of this matrix from previously computed values.  If
## the inverse is not there already it calls solve and finds it and then
## caches it for future invocations
## --------------------------------------------------------------------- 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse();
    
    if(is.null(inv)){
	# the inverse has to be calculated - does not exist
	inv <- solve(x$get())
	# set the inverse
	x$setInverse(inv)
	inv  # returns the inv
    }
    else {  # the inverse exists
	message("Getting cached inverse")
	inv
    }
}
