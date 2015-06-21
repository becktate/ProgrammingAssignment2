## R Programming - Programming Assignment #2
## These functions together cache the inverse of a matrix
## The first function creates a special matrix object that can
## be used to cache its own inverse.  The second function
## returns the cached inverse if it has already been computed
## or computes the inverse, caches it, and returns it if it
## has not already been computed



## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL

        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) i <<- inverse

        getinverse <- function() i

        list(set = set, 
			 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the special "matrix" created  
## with the above function makeCacheMatrix.  It first attempts to get the inverse from 
## the cache using the getinverse function.  If the inverse is not null, indicating that 
## the inverse has already been calculated, then a message is outputted to the screen and 
## the cached inverse is returned.  Otherwise, the matrix data is retrieved from the 
## object and the inverse is calculated using the solve() function.  The newly calculated 
## inverse is set in the cache via the setinverse function and then the calculated inverse 
## is returned.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()

        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()

        i <- solve(data, ...)

        x$setinverse(i)

        i
}
