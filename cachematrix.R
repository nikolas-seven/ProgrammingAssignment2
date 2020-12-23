## The aim of this project is to verify if the inverse of a matrix has
## already been calculated in the environment and if so, it will not 
## calculate it again

## It creates a special vector that has a function to do the following:
## set and get the value of a matrix
## and set and get the value of it's inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix by first checking if
## it has already been calculated. If so, it skips the calculation and provides
## the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
