# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the inversed matrix
# 4.  get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
#cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


#   The following function calculates (solves) the inverse the special "matrix"
#   created with the above function. However, it first checks to see if the
#   inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
#   cache and skips the computation. Otherwise, it calculates the inverse of the matrix of
#   the matrix and sets the value of the inverted matrix in the cache via the XXXXXX `setmean`
#   function.
## XXXXX Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #makeVector <- function(x = numeric()) {        
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
