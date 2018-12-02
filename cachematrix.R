## Put comments here that give an overall description of what your
## functions do

 ## This script provides the user with the ability to cache matrixes 
 ## and their inverses and the ability to get the inverses. 


 ## This function wraps a matrix with a "special list." 
 ## The "special list" contains functions to: re-set the value of the matrix,
 ## get the value of the matrix, set the value of the inverse matrix, and
 ## get the value of the inverse matrix. 

makeCacheMatrix <- function(cached_matrix = matrix()) {
    cached_inv <- NULL
    set_matrix <- function(new_matrix){
        cached_matrix <<- new_matrix
        cached_inv <<- NULL
    }
    get <- function() cached_matrix
    set_inv <- function(new_inv) cached_inv <<- new_inv
    get_inv <- function() cached_inv
    list(set_matrix = set_matrix, 
         get = get, 
         set_inv = set_inv, 
         get_inv = get_inv)
}

 ## cacheSolve accepts a matrix wrapped in a "special list" as created in 
 ## makeCacheMatrix as an argument and returns the matrix's inverse.  
 ## When called it first checks for a cached inverse, if no value exists, 
 ## we find the inverse of the matrix and cache it. The inverse of the matrix is
 ## then returned. 

cacheSolve <- function(caching_mtx, ...) {
    inv <- caching_mtx$get_inv()
    if(is.null(inv)) {
        data <- caching_mtx$get()
        inv <- solve(data)
        caching_mtx$set_inv(inv)
    }
    inv
}
