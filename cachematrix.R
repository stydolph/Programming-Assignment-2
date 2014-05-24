## Repeated calculations can take a lot of computing power, these functions
## determine the inverse of a given matrix and store it to compare rather than
## calculate if it has been caluclated already. This is done with two functions, 
## makeCacheMatrix (to chache the matrix and it's inverse) and cacheSolve to solve
## the matrix for it's inverse.

## makeCacheMarix function makes a list to set the value of the matrix, 
## get the value of the matrix and set the value of the inverse
## and get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cachesolve calculates the inverse of the matrix from makeCacheMatrix
## but first checks to see if it has been calulated first.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
            message("calculated: returning cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Test out function:
## x = rbind(c(4, 7), c(2, 6)) (from mathisfun.com)
## x
## [,1] [,2]
## [1,]    4    7
## [2,]    2    6

## matrix <- makeCacheMatrix(x)
## cacheSolve(matrix)
##  [,1] [,2]
##  [1,]  0.6 -0.7
##  [2,] -0.2  0.4

## Check to see if it was cached and will return cached value:
## cacheSolve(matrix)
## calculated: returning cached data 
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
