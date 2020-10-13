## This function calculates the inverse of a matrix.

## MakeCacheMatrix creates a "special" matrix

makeCacheMatrix <- function(x = matrix()) {
        h <- NULL
        set <- function(y) {
                x <<- y
                h <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) h <<- inverse
        getinverse <- function() h
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a "special" matrix, and then it calcules its inverse.

cacheSolve <- function(x, ...) {
        h <- x$getinverse()
        if(!is.null(h)) {
                message("getting cached data")
                return(h) #Return a matrix obtained from the cache memory.
        }
        data <- x$get()
        h <- solve(data, ...)
        x$setinverse(h)
        h ## Return a matrix that is the inverse of 'x'
}

##Test
 #This is a test for the function!
mt <- matrix(c(1,4,5,3,2,4,5,8,4), nrow = 3)
invtest <- cachesolve(makeCacheMatrix(mt))
