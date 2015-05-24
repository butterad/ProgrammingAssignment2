## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## The `makeCacheMatrix` function creates a "matrix" object that can be used 
## to cache its inverse. The special "vector" is a list containing a function 
## that will do the following:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix set up in #1
## 3. Set the value of the inverse
## 4. Get the value of the inverse set up in #2
##

makeCacheMatrix <- function(x = matrix()) {

    # Step 1. Set the value of the matrix. I created 'cache' 
    # to store the cached value. I also set it to NULL.
    cache <- NULL

    # Next, create the matrix in the working environment using the '<<-' operator
    # to assign values.

    set <- function(y) {
      x <<- y
      cache <<- NULL
    }

    # Step 2. Get the value of the matrix set up in Step 1.
    get <- function() x

    # Step 3. Set the value of the inverse by inverting the matrix and then store
    # it in `cache`. Compute the inverse of the matrix with the 'solve' function.
    setInvMatrix <- function(solve) cache <<- solve

    # Step 4. Get the inverted matrix from 'cache'
    getInvMatrix <- function() cache

    # Return a list of the created functions.
    list(set = set,
         get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
 
}


## Write a short comment describing this function

## The `cacheSolve` function computes the inverse of the matrix created by
## `makeCacheMatrix`. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## Get the inverted matrix that is stored in `cache`
    cache <- x$getInvMatrix()

    ## Check to verify that the inverted matrix exists
    if(!is.null(cache)) {

      ## Display the matrix data to the console
      return(cache)

    }

    ## If the inverted matrix does not exist, it needs to be created. 
    matrixNew <- x$get()

    # Set and return the inverse of `matrixNew` into `cache`
    cache <- solve(matrixNew, ...)
    x$setInvMatrix(cache)

    ## Display the matrix data to the console
    return(cache)

}
