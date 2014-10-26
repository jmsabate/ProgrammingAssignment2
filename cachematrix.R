## Functions below calculate and cache the inverse of a matrix
## makeCacheMatrix: just returns a list of functions in order to set sand get
##                  the matrix and its inverse in/from the cache
## cacheSolve:      obtains the inverse of a matrix. First of all checking if the
##                  inverse is already calculated previously or
##                  caculating the inverse and setting it

## makeCacheMatrix: returns a list of funtions:
##                  set:   to set the matrix (and making NULL its inverse)
##                  get:   to obtain the matrix to be inverted
##                  setinverse: to set the inverse of the matrix
##                  getinverse: to get the inverse of the matrix
## Recommendation: in your main program assign > m <- makeCacheMatrix()
##                 so then you can use m$set(matrix) and cacheSolve(m)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# cacheSolve expects as a first argument an object-list of functions defined in makeCacheMatrix. Functions are:
# (see makeCacheMatrix) and returns the inverse of the matrix
#
# Previous to call cacheSolve it is supposed:
# 1.- There is a matrix. Let's say "matriu"
# 2.- It has been called makeCacheMatrix to obtain the list of functions
#     Let's say m <- makeCacheMatrix()
# 3.- It has been cached the original matrix: >m$set(matriu)
# Afther doing that, you can call > cacheSolve(m). cacheSolve does:
#     First of all this function tries to obtain the inverse, if it's cached returns it
#     if it's not, gets the matrix (previously set) and obtains the inverse,
#     caches it and returns it
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        
        # WARNING: "For this assignment, assume that the matrix supplied is always invertible."
        #          To generalize the code,it should be controled if "data" matrix has inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
