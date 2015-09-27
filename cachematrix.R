## Put comments here that give an overall description of what your
## functions do

## The two functions 'makeCacheMatrix' and 'cacheSolve'
## is used to improve the performance of the matrix
## operation 'solve'. This is done by storing the inverse matrix
## in the list type returned by 'makeCacheMatrix'. The cacheSolve
## will lookup if the inverse matrix is already calculated, and if so
## return the inverse directly, and otherwise calculate the inverse
## and then store the result in the parent environment and return the 
## just calculated inverse matrix as a last step.

## Write a short comment describing this function

## The function 'makeCacheMatrix'
## takes a square matrix as argument and return a list
## containing the matrix and set/get functions for
## storing the invers matrix.
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(invm) invMatrix <<- invm
        getinv <- function() invMatrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## The function cacheSolve takes the special matrix (a list)
## produced by 'makeCacheMatrix' and return the already
## calculated inverse matrix if it exists, and otherwise
## do the inverse operation and stores the result
## in the parent environment of the list returned
## from the function 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting inversed matrix")
                return(m)
        }
        matrix <- x$get()
        invMatrix <- solve(matrix,...)
        x$setinv(invMatrix)
        invMatrix
}

## Test run:

# > m
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > invM <-makeCacheMatrix(m)
# > cacheSolve(invM) 
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(invM)
# getting inversed matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 

