## makeCacheMatrix and cacheSolve work together to save calculation time by
## preventing the inverse of a matrix to be calculated more than once.

## The makeCacheMatrix function contains a list of functions that:
## 1) take in a matrix (setmatrix)
## 2) retrieve the matrix (getmatrix)
## 3) take in the inverse of the matrix (setinverse)
## 4) retrieve inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function (y){
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve does one of two things:
## 1) If the inverse has already been computed, it outputs the inverse
## 2) If not, the inverse is computed and output

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
