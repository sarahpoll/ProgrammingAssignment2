##This code contains two functions
## 1. makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

## makeCacheMatrix makes a special "matrix" which is really a list to:
### 1. set the value of the matrix
### 2. get the value of the matrix
### 3. set the value of the inverse
### 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the value of the special matrix created with createCacheMatrix, 
## checks to see if inverse has been calculated (if yes, it will get the 
## inverse from the cache), if not, it calculates the inverse with solve()

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
}

