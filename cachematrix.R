        ##############################################################################
        # The first function, `makeCacheMatrix` creates a special "vector", which is #
        # really a list containing a function to                                     #
        #                                                                            #
        # 1.  set the value of the matrix                                            #
        # 2.  get the value of the matrix                                            #
        # 3.  set the value of the inversmatrix                                      #
        # 4.  get the value of the inversematrix                                     #
        ##############################################################################

# Create the function
makeCacheMatrix <- function(x = matrix()) {
        #check if there alreay a solution for the matrix
        m <- x$SolveMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #otherwise caculate inverse matrix
        else{
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
        }
        m
}

        ###################################################################################################
        #   The function "cacheSolveM" calculates (solves) the inverse the special "matrix"           #
        #   created with the above function. However, it first checks to see if the                       #
        #   inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the      #
        #   cache and skips the computation. Otherwise, it calculates the inverse of the matrix of        #
        #   the matrix and sets the value of the inverted matrix in the cache via the XXXXXX `setmean`    #
        #   function.                                                                                     #
        ###################################################################################################


cacheSolveM <- function(x, ...) {
  #create NULL vectore
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  SolveMatrix <- function() m
  list(set = set, get = get,
       setInvers = setInvers,
       SolveMatrix = SolveMatrix)
}
