

## Creates new Cache Matrix, a Matrix with a Cache for inverse values for optimizing calculations where the inverse is needed many times
makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) c <<- inverse
    getinverse <- function() c
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of a CacheMAtris and uses the Cache to improve performance

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getinverse()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    data <- x$get()
    c <- solve(data, ...)
    x$setinverse(c)
    c
}

## for easy testing:
# mat <- matrix(c(2,2,3,2),nrow=2, ncol=2)
# solve(mat)
# cm$set(mat)
# cm$get()
# cacheSolve(cm)
## will print "getting cached data"
# cacheSolve(cm)
