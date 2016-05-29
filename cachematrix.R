## Put comments here that give an overall description of what your
## functions do
## 
## cache matrix 
## The first function, CacheMatrix creates a special "vector", which is really a list containing a function to
## set the values of the matrix
## get the values of the matrix
## set the values of the inverse
## get the values of the inverse

## Write a short comment describing this function
## set is assigned to x matrix
## get is used to retrieve matrix back
## setinverse is used to assign the matrix inverse to variable inv
## getinverse is used to retrieve the matrix inverse from variable inv

CacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## 
## The following function calculates the inverse of the matrix 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix data and sets the value of the inverse in the cache 
## via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }		
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
