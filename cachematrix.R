## makeCacheMatrix takes a matrix as its arg, computes its inverse,
## and stores the result in the cache

makeCacheMatrix <- function(x = matrix()) {
        ##
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve takes arg of type "makeCacheMatrix" (class = matrix), and checks
## if the inverse of this matrix exists in the cache. If not, inverse is calculated
## and stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## ***IMPORTANT: x must be of type "makeCacheMatrix"
        m <- x$getSolve() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
