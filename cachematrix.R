## The following functions allow us to understand the importance of "<<-" operator
## The "<<-" operator helps us to cache data to an environment other than the current environment
## In case computation is to be done on an object who's value does not change, caching the computed value saves computational time.


##############################------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse.
## variable "i" contains the inverse of the input square matrix "x"

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}
################################------------------------------------------------------


################################------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated, then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse ()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
################################------------------------------------------------------

