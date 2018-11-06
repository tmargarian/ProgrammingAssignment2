## This R file contains two functions required for completeing the peer-graded assignment for the
## "R programming" course 

## The makeCacheMatrix is a function which returns a list of 4 functions (setters and getters for a matrix and inverted matrix)
## and contains a cache with previously saved inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        cached_inv_matr <- NULL          # each time we call makeCacheMatrix fuction we have to clean the cache at first
        set_matr <- function(y) {        # this function sets the x value (we can set/change it via x$set)
                x <<- y
                cached_inv_matr <<- NULL #changing the cache requires to remove the previous one
        }
        get_matr <- function() x         # this function allows us to get the x matrix
        set_inv_matr <- function(inv_matr) cached_inv_matr <<- inv_matr # this function sets the inverted matrix to the cache
        get_inv_matr <- function() cached_inv_matr     # this function allows us to get the inverted x matrix from cache
        list(set_matr = set_matr, get_matr = get_matr,
             set_inv_matr = set_inv_matr,
             get_inv_matr = get_inv_matr)
}


## The cacheSolve function returns the inverted matrix from cache (if applicable) or calculates it again

cacheSolve <- function(x, ...) {
        cached_inv_matr <- x$get_inv_matr() #get the cache
        if(!is.null(cached_inv_matr)) { # if there is a cached matrix (cache != NULL)
                message("getting cached inverted matrix")
                return(cached_inv_matr) # return it
        }
        message("inverting matrix...") # if there is no cached matrix (cache == NULL)
        data <- x$get_matr() #get the original matrix
        cached_inv_matr <- solve(data,...) #calculate the inverted one
        x$set_inv_matr(cached_inv_matr) #save the inverted matrix into the cache
        cached_inv_matr #return the inverted matrix
}

#Please note that input matrix is considered invertible by default 