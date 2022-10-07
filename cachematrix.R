## I create a special object that can be cached.
## In comparison to the example given i mostly changed variables.

## I changed all variables to the new context of matrix and inverse instead of mean.

makeCacheMatrix <- function(x = matrix()) {
        invertedObj <- NULL
        set <- function(y) {
                x <<- y
                invertedObj <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invertedObj <<- inverse
        getInverse <- function() invertedObj
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse and if already computed, delivers the cached inverse matrix. Mostly changing variables like above

cacheSolve <- function(x, ...) {
        invertedObj <- x$getInverse()
        if (!is.null(invertedObj)) {
                return(invertedObj)
                message("this is cached data")
        }
        data <- x$get()
        invertedObj <- solve(data, ...)
        x$setInverse(invertedObj)
        invertedObj
}
