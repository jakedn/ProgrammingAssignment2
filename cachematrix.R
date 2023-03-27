## This source file contains two functions that work together to help |
## a user cache a matrix inverse and retrieve the cached values


## This function returns a vector of functions that can set a matrix value
## calculate its inverse and cache it for future use

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
      x <<- y
      invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This function allows you to get the inverse of matrix 'x' by 
## calculating it the first time its called and caching it.
## returning the cached value for every future call

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
      message("getting cached data")
      return(invmat)
    }
    mat <- x$get()
    invmat <- solve(mat, ...)
    x$setinverse(invmat)
    invmat
}
