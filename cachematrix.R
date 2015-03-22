## Caching Matrix inverse operation

## Creates a special Matrix object that supports inverse cacheing
makeCacheMatrix <- function(x = matrix()) {
  # The cached inverse
  i <- NULL
  set <- function (y){
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inv from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the cache via the `setInverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        mat <- x$get()
        i <- solve(mat)
        x$setInverse(i)
        i
}
