## The two functions below calculate and put in cache the inverse matrix
## of a given matrix. The user needs to create a square matrix, then
## run the makeCacheMatrix function (with the created square matrix as
## an argument) and then run the second function. 

## The makeCacheMatrix function creates a special vector which is really a
# list containing a function to

# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)

}

# The following function calculates the inverse matrix of the special "vector" created 
# with the above function. However, it first checks to see if the inverse matrix 
# has already been calculated. If so, it gets the invert matrix from the cache and skips 
# the computation. Otherwise, it calculates the inverse matrix and sets the 
# value of the calculation in the cache via the setmean function.


cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        ## Return a matrix that is the inverse of 'x'
        #m <- getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinvert(m)
        m

}
