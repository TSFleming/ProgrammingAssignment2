## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and creates a list of 4 functions:
##      get - prints original matrix
##      set - allows user to set a new matrix (this whill then change the get function)
##      setinverse - allows user to define their own inverse of matrix
##      getinverse - prints the inverse that has currently been calculated for the original matrix
##                      - i.e. prints "NULL" if no inverse has been calculated, or prints actual
##                              inverse that has been calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## This function checks if an inverse has already been calculated for the input matrix. If
## the inverse has already been calculated, it will be retrieved from the cache and printed
## (along with the message "getting cached data"). If no inverse exsits, it will be 
## calculated, stored in cache and printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
