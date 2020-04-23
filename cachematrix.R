makeCacheMatrix <- function(x = matrix()) {
  
  # makes a cache matrix from a given matrix
  # assign the value NULL for the first initialization
  
  cacheMatrix <- NULL
  
  # define the function named 'setMatrix'
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # define the method named 'getMatrix'
  
  getMatrix <- function() x
  
  # define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # define the method named 'getCache'
  # return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
  
}


cacheSolve <- function(x, ...) {
  
  
  # return the inverse of a given matrix utilizing the cache
  
  cacheMatrix <- x$getCache()
  
  # if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  
  # if the content is empty: 
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
