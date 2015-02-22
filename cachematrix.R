## Caching the inverse of a matrix
##
## These 2 functions work together to cache and provide the inverse of a matrix
## The function makeCacheMatrix returns a list of objects that are all functions, that work in unison to provide
## a cache mechanism for the inverse of a matrix.
## The function cacheSolve uses the object list returned by makeCacheMatrix in order to calculate & cache the
## inverse of the matrix
##
##
## makeCacheMatrix returns a list, in which every object is a function, that will be used by cacheSolve to
## calculate and cache the inverse of the matrix provided

makeCacheMatrix <- function(x = matrix()) {
      # Variable that stores the cached inverse of the matrix
      INV <- NULL

      # Function to set a new value of the matrix
      set <- function(y) {
            # Update the value of our matrix with the new value provided
            x <<- y
            # Remove the previous cached inverse
            INV <<- NULL
      }

      # Function to return the value of the matrix
      get <- function() x

      # Function to update cache with the new inverse of the matrix
      setinverse <- function (inv) {
            INV <<- inv
            # Slight improvement over the original implementation. Return the calculated value so the setinverse
            # function can be used outside of cacheSolve with the same result
            INV
      }

      # Function to return the cached inverse of the matrix

      getinverse <- function() INV

      # Return the list of function objects we'll use to cache and provide the inverse of the matrix
      list(
            set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse
      )

}


## This function either caches the inverse of a matrix and returns it, or if the inverse has been
## previously calculated returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) { # Our cached inverse is not null, so return it
            message("getting cached inverse")
            return (inv)
      }
      # Cache is null. Time to calculate the inverse
      x$setinverse(
            solve(x$get(), ...))

}
