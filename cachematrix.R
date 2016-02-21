## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## x <- matrix( 1:4 , 2, 2)
          ## This function creates a special "matrix" object that can cache its inverse.
          
          i <- NULL ##initialize i 
          set <- function(y) { ##set matrix to compute
                    x <<- y 
                    i <<- NULL ##initialize i to get new inverse matrix
          }
          get <- function() x ##get original matrix
          setinverse <- function(matrix) i <<- matrix 
          getinverse <- function() i
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          ##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
          ##If the inverse has already been calculated (and the matrix has not changed), 
          ##then the cachesolve should retrieve the inverse from the cache.
          i <- x$getinverse()
          if(!is.null(i)) {
                    message("getting cached data")
                    return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
