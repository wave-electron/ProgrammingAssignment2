## Put comments here that give an overall description of what your
## functions do.

## Write a short comment describing this function
## This function creates a matrix to chage its inversible. 

makeCacheMatrix <- function(x = matrix()) {

          inv <- NULL
          set <- function(y) {
            
              x <<- y
              m <<- NULL
            
          }
          get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
  
}


## Function CacheSolve below calculates the inverse of the matrix created by Function makeCacheMatrix
## above. If the inverse is already calculated (and the matrix has not changed), then it retrieves inverse
## from cache.
## You can read/write the matrix and the inverse of the matrix using these methods, for example:
## > matrixA <- matrix(c(3,4,1,0), nrow = 2, ncol =2)   <= An invertiable test matrix
## > m1 <- makeCacheMatrix(matrixA)                     <= class(m1) is a list with names set,get,setinverse,getinverse
## > matrixA <- m1$get()                                <= m1 has accessing methods: set(), get(), setinverse(),getinverse()    
## > m1$setinverse(solve(matrixA))                      <= set inverse of matrixA
## > matrixInverse <- m1$getinverse()                   <= gets the set inverse and assigns to matrixInverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getinverse()
         if(!is.null(inv)) {
                 message("getting cached data") 
                 return(inv)
         }
         data <- x$get()
         inv <- solve(data, ...)
         x$setinverse(inv)
         inv
}
