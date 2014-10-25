##The two functions are useful to handle a matrix together with its inverse in order to compute the inverse of a given matrix only once (since this is a computationally expensive operation)

## This first function creates an object from a matrix x that is made of 4 functions: set (create the object from the matrix), get (print the matrix associated to the object), setinverse (cache the inverse of the matrix), getinverse (print the inverse)

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


## This function takes the object associated to a matrix and print the inverse of the matrix. In case it was already computed, it uses the cached inverse and print it together with a message. Otherwise it computes the inverse, cache it and print it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        N <- x$get()
        m <- solve(N,...)
        x$setinverse(m)
        m
}
