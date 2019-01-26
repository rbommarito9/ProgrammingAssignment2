##Here I have written two functions "makeCacheMatrix" 
##and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (we are assuming the matrix is invertible)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

##cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated, 
##then the cachesolve should retrieve the inverse from the cache and does not
##calculate the inverse again. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

##To test the two functions:
z <- matrix(rnorm(25), 5, 5)
z1 <- makeCacheMatrix(z)
cacheSolve(z1)
