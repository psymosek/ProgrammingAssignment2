## These utility functions create an R-object which stores a matrix and will calculate the
## inverse of the matrix only if it has not been calculated previously. Otherwise the cached
## version of the inverse is retrieved.

## This function creates a list of interface functions for the cache. The set function sets
## the matrix cache. The get function retrieves the matrix from the cache. The set_inverse
## function sets the cache to the inverse of the matrix supplied as an argument. The 
## get_inverse function retrieves the inverse of the matrix from the cache.

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(imatrix) inv_matrix <<- imatrix
    get_inverse <- function() inv_matrix
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function retrieves the inverse of a matrix from the cache (created by the function
## makeCacheMatrix). If the top-left element of the inverse of the matrix retrieved from
## the cache is NULL, then the inverse of the matrix is recalculated. Otherwise, the value
## retrieved from cache is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$get_inverse()
    if(!is.null(inv_matrix[1][1])) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$set_inverse(inv_matrix)
    inv_matrix
}
