# operator which can be used to assign a value to an object in an environment 
# that is different from the current environment. Below are two functions that 
# are used to create a special object that stores a numeric vector and cache's its mean.

makeVector <- function(x = numeric()) { 
        m <- NULL                               # sets the mean to NULL
        set <- function(y) {                    # set the value of the vector
                x <<- y                         # ste a function to set the vector x to a new vector y 
                m <<- NULL                      # it resets the mean to NULL
        }
        get <- function() x                     # get the value of the vector
        setmean <- function(mean) m <<- mean    # set the value of the mean
        getmean <- function() m                 # get the value of the mean
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# the following function calculates the mean of the special "vector" created 
# with the above function
cachemean <- function(x, ...) {
        m <- x$getmean()        # it first checks to see if the mean has already been calculated
        if(!is.null(m)) {       # If so, it gets the mean from the cache and skips the computation
                message("getting cached data")
                return(m)
        }
        data <- x$get()      
        m <- mean(data, ...)    # Otherwise, it calculates the mean of the data
        x$setmean(m)            # sets the value of the mean in the cache via the setmean function.
        m
}