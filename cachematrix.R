
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix<- NULL
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) cacheMatrix <<- solve
  getInverse <- function() cacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getInverse()         #query the x matrix's cache         
  if(!is.null(cacheMatrix)) {           #if there is a cache
    message("getting cached data") 
    return(cacheMatrix)                 #just return the cache, no computation needed
  }
  data <- x$get()                       #if there's no cache
  cacheMatrix <- solve(data, ...)       #we actually compute them here
  x$setInverse(cacheMatrix)             #save the result back to x's cache
  cacheMatrix                           #return the result
}
