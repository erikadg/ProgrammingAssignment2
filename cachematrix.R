## Put comments here that give an overall description of what your
## functions do

## Below is a function 'makeCacheMatrix' that is used to create a special 
## "matrix" object that stores a matrix and cache's its inversion

makeCacheMatrix <- function(x = matrix()) {     
        inverse <- NULL                         # set the inversion to NULL as a placeholder
        set <- function(y) {                    # it defines a function
                x <<- y                         # to set the matrix x to a new matrix y 
                inverse <<- NULL                # It resets the matrix inversion to NULL
        }
        get <- function() x                     # It returns the matrix x
        setinverse <- function(solve) inverse <<- solve    # set the inversion, 'inverse' to solve
        getinverse <- function() inverse                 # It returns the matrix inversion
        list(set = set, get = get,     
             setinverse = setinverse,
             getinverse = getinverse)           # it creates a list containing all the functions,
                                                # i.e. set, get, setinverse, getinverse, defined
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()               # checks if the inversion has already been calculated
        if(!is.null(inverse)) {                 # If so, it gets the inverse from the cache 
                message("getting cached data")  # print the message to the console
                return(inverse)                 # and skips the computation
        }
        data <- x$get()                         # Otherwise, it takes the matrix
        inverse <- solve(data, ...)             # and it calculates the inversion
        x$setinverse(inverse)                   # sets the inverse in the cache via setinverse function.
        inverse                                 # it returns the result
} 

