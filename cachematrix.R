## Computing the inverse matrix can be a time consuming calculation making it
##   advantageous to cache the result for later retrieval. These functions set
##   up a system for the purposes of cacheing a pre-calculated inverse matrix.

## makeCacheMatrix sets up an environment to cache the inverse of a square matrix. It
##   contains functions which save the value of a matrix (set), allow the matrix to be
##   retrieved (get), save the value of the inverse matrix (setInverseMatrix), and
##   retrieve the value of the inverse matrix (getInverseMatrix).

makeCacheMatrix <- function(x = matrix()) {
      ## My additions
            m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverseMatrix <- function(solve) m <<- solve
      getInverseMatrix <- function() m
      list(set = set, get = get,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}

## cacheSolve retrieves a cached inverted matrix, and calculates the inverse if 
##    the matrix has changed since the inverse was cached.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverseMatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverseMatrix(m)
      m
}
