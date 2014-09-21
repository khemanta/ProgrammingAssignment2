## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Descriptions about the "makeCacheMatrix" 
# makeCacheMatrix implements a function that stores a matrix and also stores 
# inverse of the matrix in cached memory. The following four methods are implemented 
#  setMat      	sets the values of the matrix
#  getMat      	gets the values of the matrix
#  cacheInvert  gets inverse of the square and invertible matrix stored through solve
#  getInvert    gets the cached value of square and inverted matrix

# Now the implementation part
# makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
		# define and initialized cacheMat with NULL to avoid garbage values
        cacheMat <- NULL
        
        # assigning value to the matrix 
        setMat <- function(val) {
                x <<- val
        # flushing cached memory of matrix with NULL assignment
                cacheMat <<- NULL
        }

        # getMat method to return the matrix
        getMat <- function() {
                x
        }

        # caching the matrix cacheMat with matrix inverted values 
        cacheInvert <- function(solve) {
                cacheMat <<- solve
        }

        # getInvert returns the value of the cacheMat store with invertible matrix
        getInvert <- function() {
                cacheMat
        }
        
        # Listing all the methods 
        list(setMat = setMat, getMat = getMat, cacheInvert = cacheInvert, getInvert = getInvert)
}


# The following function calculates the inverse of a "special" matrix created with makeCacheMatrix() 

# cacheSolve function
cacheSolve <- function(x, ...) {
        # get the cached value
        invMat <- x$getInvert()
        # if a cached value exists return it
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        # else compute the inverse of getMat and assign to cache using solve()
        data <- x$getMat()
        invMat <- solve(data)
        x$cacheInvert(invMat)
        
        # return the inverted matrix
        invMat
}
