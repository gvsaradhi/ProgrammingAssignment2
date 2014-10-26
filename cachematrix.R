
## OVERALL DESCRIPTION: 
# The two functions below: 'makeCacheMatrix' and 'cacheSolve' are used to create
# a special object that stores a numeric matrix and caches its inverse.
# Test code demonstrates the utility of these two functions



## makeCacheMatrix: Functionality
# makeCacheMatrix creates a special matrix, which is really a list containing a
# function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
     inv_matrix <- NULL
     set <- function(y) {
          x <<- y
          inv_matrix <<- NULL
     }
     get <- function() x
     set_inv_matrix <- function(imat) inv_matrix <<- imat
     get_inv_matrix <- function() inv_matrix
     list(set = set, get = get,
          get_inv_matrix = get_inv_matrix,
          set_inv_matrix = set_inv_matrix)

}


## cacheSolve: Functionality
# The following function calculates the inverse of the special matrix created with
# the above function 'makeCacheMatrix'. 
# It does the following:
# 1. Checks to see if the inverse of the matrix has already been calculated.
# 2. If it is already calculated, it gets the cached inverse and skips the computation.
# 3. If the inverse is not yet calculated, it calculates the inverse and sets the value
#    of the inverse in the cache via the set_inv_matrix function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv_matrix= x$get_inv_matrix()
     if(!is.null(inv_matrix)){ # if inv_matrix is cached
          message("getting cached inverse") #this gets printed when cached inverse is returned
          return(inv_matrix)
     }
     data= x$get()
     inv_matrix= solve(data,...) #computes the inverse of the matrix
     x$set_inv_matrix(inv_matrix) # caches the computed inverse
     message("returning computed inverse") #this gets printed when inverse is computed
     inv_matrix
     
}

## TEST THE CODE
# create an identity matrix for simplicity. Note: Inverse of identity matrix is identity matrix
input_matrix= diag(3)
# create a special cache matrix of input_matrix
special_input_matrix= makeCacheMatrix(x=input_matrix)
# check if the inverse exists
special_input_matrix$get_inv_matrix() #now it should return NULL and 
# compute the inverse using cacheSolve - it should print "returning computed inverse
computed_inv_matrix = cacheSolve(special_input_matrix) #this will return the inverse and also caches the inverse
# now see if it computes or gets the cached value - it should print " getting cached inverse"
cached_inv_matrix = cacheSolve(special_input_matrix)


