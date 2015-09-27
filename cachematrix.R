## Computing the inverse matrix can be a time consuming calculation making it
##   advantageous to cache the result for later retrieval. These functions set
##   up a system for the purposes of cacheing a pre-calculated inverse matrix.

## makeCacheMatrix sets up an environment to cache the inverse of a square matrix. It
##   contains functions which save the value of a matrix (set), allow the matrix to be
##   retrieved (get), save the value of the inverse matrix (setInverseMatrix), and
##   retrieve the value of the inverse matrix (getInverseMatrix).

makeCacheMatrix <- function(x = matrix()) {
      ## define the original value of the inverse matrix. Setting as null will allow 
      ##    to test if the value is cached.
      m <- NULL
      ## This function saves the value of the matrix. If the matrix is changed after 
      ##    originally set, m is reset to null so that the inverse must be re-solved.
      ##    If the matrix were re-set but hadn't actually changed then the cached
      ##    inverse matrix is valid. Would it be faster to compare the new matrix and
      ##    the original and decide to re-solve the inverse, or always re-solve the
      ##    inverse. The unknown here is the time it takes to compare matrices vs.
      ##    re-solve the inverse.
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## the get function returns the value of the matrix, in case the inverse has not yet
      ##    been cached
      get <- function() x
      ## the setInverseMatrix calls solve to do the calculation of the inverse matrix
      setInverseMatrix <- function(solve) m <<- solve
      ## the getInverseMatrix functions returns the value of the cached inverse matrix
      getInverseMatrix <- function() m
      ## the return of the makeCacheMatrix is a list of the functions defined. This form
      ##    allows access to particular elements by name using the extract operator '$'
      list(set = set, get = get,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)
}

## cacheSolve retrieves a cached inverted matrix, and calculates the inverse if 
##    the matrix has changed since the inverse was cached. It takes as input a
##    makeCacheMatrix object which carries a list of the necessary initialized
##    functions

cacheSolve <- function(x, ...) {
      ## Fetch the cached value 'm'
      m <- x$getInverseMatrix()
      ## m is initialized to null but if the inverse has been solved, and the input matrix
      ##    has not changed, then the inverse matrix is the cached value and is directly 
      ##    returned so that no re-calculation is performed
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if m is null then the inverse matrix must be re-solved. Fetch the data, solve m,
      ##    and cache the new value of m. Return the newly solved value of m.
      data <- x$get()
      m <- solve(data, ...)
      x$setInverseMatrix(m)
      m
}
