## These 2 functions are used to create a matrix and to return and cache the
## inverse of the matrix, then if we want to return the inverse of the matrix in
## future it will return the cached version instead of calculating it again.

## This function (makeCacheMatrix) takes a matrix and first checks that this is
## a square matrix it then creates the following 4 functions
## 1. to set the matrix
## 2. to get the matrix
## 3. to set the inverse of the matrix
## 4. to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        if(dim(x)[1] != dim(x)[2]) {      #check if this isn't a square matrix
                stop('Not a square matrix! Dimensions (',
                     dim(x)[1], ' x ', dim(x)[2], ' )')
                # Error message if this isn't a square matrix
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


## This function (cacheSolve) checks to see if we have already calculated the
## inverse of the cached matrix we'd created earlier with the makeCacheMatrix
## function, if so it returns the already calculated inverse matrix, otherwise
## it sets the inverted object to the inverse of the matrix and returns it.

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
