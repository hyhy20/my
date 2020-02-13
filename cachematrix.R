makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<-inverse
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.na(m)){
        message("Got data")
        return(m)
    }
    newm <- x$get()
    m <- solve(newm, ...)
    x$setinv(m)
    m
}
