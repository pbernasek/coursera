## Below are a pair of functions that cache the inverse of a matrix. 

## makeCacheMatrix creates a special matrix that can cache its inverse using the solve() function

makeCacheMatrix <- function(x = matrix()) {
	        m <- NULL ##initialize the "inverse" variable to NULL so that the function can run on first time
        set <- function(y) {
                x <<- y  ##set x for the parent environment to y
                m <<- NULL ##set m for the parent environment to NULL
        }
        get <- function() x  
        setinv <- function(solve) m <<- inv
        getinv <- function() m
        matrix(set = set, get = get,
             setinv = setinv,
             getinv = getinv) ##lists values of functions calculated above in a matrix 
}


}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above.  If inverse has already been calculated and the matrix has not changed, then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinv() ##goes to the x environment and assigns m value from that environment 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## if the x environment has been evaluated before, print message and return cached value
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m  
        ## if the particular x hasn't been evaluated, then pull x into the local variable data, and calculate the inverse with the solve function
}

}
