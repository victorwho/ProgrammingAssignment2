## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates CacheMatrix with 4 functions (get, set, getinverse,setinverse) and 2 data objects (x,m)
## x is our matrix, m is the variable for storing the inverse and passing it on to the parent environment
## get: shows the matrix
## getinverse: shows the inverse (if it has been calculated)
## setinverse: assigns the inverse to m in the parent environment
## set: allows to change values without creating a new object

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


## Write a short comment describing this function
## cacheSolve checks if there is an inverse (stored as m) that is not null: if m is not null then it retrieves its value and shows appropriate message. 
## If m is null then there is no inverse, so it calculates the inverse and stores it in setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Inverse is cached. Retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
