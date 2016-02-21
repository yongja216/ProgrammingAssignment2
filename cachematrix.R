## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## x <- matrix( 1:4 , 2, 2)
          
          i <- NULL ## assign NULL so that calculate inverse matrix at cacheSolve function
          set <- function(y) { ##set matrix to compute
                    x <<- y ## can get the new input matrix at get function
                    i <<- NULL ## assign NULL so that calculate inverse matrix at cacheSolve function
          }
          get <- function() x ##get original matrix
          setinverse <- function(matrix) i <<- matrix ## assign matrix to i, cached data
          getinverse <- function() i   ##get inverse matrix
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
          if(!is.null(i)) {   ##get cached data(inverse matrix) if input is not changed
                    message("getting cached data")
                    return(i)
          }
          data <- x$get() ## get the new input matrix
          i <- solve(data, ...) ##calculate inverse matrix
          x$setinverse(i) ## set new calculated inverse matrix to i
          i
}
