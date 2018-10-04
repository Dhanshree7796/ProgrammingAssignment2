

## makeCacheMatrix creates a list containing a function to..
#1. Set the value of Matrix
#2. Get the value of the matrix
#3. Set the inverse value of the matrix
#4. Get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) 
                {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function()
                inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}

## The function below returns the inverse of the matrix.
#It first checks if the inverse has already been computed.
#If inverse has already been computed, it gives the result and not performs the computation.
#If not, it computes the inverse, sets the value in cache via Setinverse.

### Following function assuems thet the matrix is always invertible.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

##### SAMPLE RUN #######

> x = rbind(c(1, -1/4),c(-1/4,1))
> m = makeCacheMatrix(x)
>m$get()
[,1]  [,2]
[2,] -0.25  1.00
[2,] -0.25  1.00









