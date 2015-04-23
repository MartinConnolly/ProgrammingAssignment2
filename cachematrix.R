## These 2 functions are used to create a matrix and to return and
## cache the inverse of the matrix, then if we want to return the
## the inverse of the matrix in future it will return the cached
## version instead of calculating it again

## This function (makeCacheMatrix) takes a matrix and first checks
## that this is a square matrix it then creates the following 4
## functions
## 1. sets the matrix
## 2. gets the matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        num_dim <- length(dim(x)) #find number of dimensions of matrix
        if(dim(x)[1] != dim(x)[2]) {
                stop('Not a square matrix! Dimensions (',
                     dim(x)[1], ' x ', dim(x)[2], ' )')
        }
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinverted <- function(inverse) inverted <<- inverse
        getinverted <- function() inverted
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)

}


## This function (cacheSolve) checks to see if we have already
## calculated the inverse of the cached matrix we'd created earlier
## with the makeCacheMatrix function, if so it returns the already
## calculated inverse matrix, otherwise it sets the inverted variable,
## which is the inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getinverted()
        if(!is.null(inverted)) {
                message("retrieving cached inverse of matrix ...")
                return(inverted)
        }
        my_matrix <- x$get()
        inverted <- solve(my_matrix, ...)
        x$setinverted(inverted)
        inverted
}
