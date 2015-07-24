## makeCacheMatrix transforms an input square invertible matrix 
## into a matrix that can cache its inverse 

## use two variables, one to store input matrix and one to store
## inverted matrix (initially set to null)

makeCacheMatrix <- function(x = matrix()) {
     # set m to null 
     m <- NULL
     setmatrix <- function(y) {
     x <<- y        ## cache inputted matrix
     m <<- NULL     ## set value for inverted matrix to NULL
     }
     getmatrix <- function() x                 # retrieve matrix
     setinverse <- function(solve) m <<- solve # invert matrix
     getinverse <- function() m                # get inverted matrix
     ## store functions in a list for use in cacheSolve
     list(setmatrix = setmatrix, getmatrix = getmatrix,     
          setinverse = setinverse, getinverse = getinverse) 
}

## cacheSolve takes as input the object created in makeCacheMatrix
## It computes the inverse of the object. If that calculation has already 
## occurred, it reuses the cached version.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()            ## get inverse of original matrix
     if(!is.null(m)) {                   ## check for cached value
          message("getting cached data") ## use if populated
          return(m)
     }
     data <- x$getmatrix()  ## if not cached, get matrix input
     m <- solve(data, ...)  ## invert input matrix
     x$setinverse(m)        ## set the inverted matrix to cache
     m                      ## print inverted matrix 
}
