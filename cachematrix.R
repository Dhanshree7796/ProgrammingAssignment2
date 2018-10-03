## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
 set <- function(y) {
 x <<- NULL
 }
 get <- function() x
 setInverse <- function(solveInverse) inv <<- solveMatrix
 getInverse <- function() inv
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
