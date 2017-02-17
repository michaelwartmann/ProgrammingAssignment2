## Calculating the inverse of a matrix is computationally expensive. The big O expense is between O(n2) and O(n3)
## The two functions below allow to cache a matrix instead of calculating eat repeatedly if it is needed more than one time.

## This function creates a matrix object.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of a matrix. If the matrix has earlier been computed and not changed in the meantime
## then this function takes the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
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