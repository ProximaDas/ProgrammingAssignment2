## The below set of functions, makeCacheMatrix and cacheSolve cache 
## inverse of matrices that have already been computed to save 
## computational time.
## Example usage: 
## b <- matrix(c(1,-1/4,-1/4,1),ncol=2,nrow=2)
## result <- makeCacheMatrix(b)
## cacheSolve(result)

## makeCacheMatrix is a function that accepts a matrix as an input and
## returns a list of 4 functions: set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
}


## cacheSolve accepts the result of makeCacheMatrix and checks whether 
## the inverse of the matrix is in cache. If not, it calculates inverse
## of the matrix by using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        ## Check if inverse of the same matrix is already calculated.
        ## If yes, then return the cached data
        if (!is.null(m)) {
          message ("getting cached data")
          return(m)
        }
        
        data <- x$get()
        ## Calculate inverse of matrix using solve function.
        m <- solve(data,...)
        x$setinv(m)
        
        m
        

}
