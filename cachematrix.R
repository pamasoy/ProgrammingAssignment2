## Caching the Inverse of a Matrix.

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setMatrix <- function(y) {
                x <<- y
                inv <<- NULL}
        getMatrix <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function () inv
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached invertible matrix")
                return(inv)}
        mat<- x$getMatrix()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        return(inv)
}


## Commands to test the functions (example)(step by step)

## source('~/Desktop/Coursera r studio/week 3/Repo/ProgrammingAssignment2/cachematrix.R')
## matrixtest_1 <- makeCacheMatrix(matrix(1:4, 2, 2))
## matrixtest_1$getMatrix()
## matrixtest_1$getInverse()
## cacheSolve(matrixtest_1)
## cacheSolve(matrixtest_1)     ##getting the stored mean