## Stores any invertible matrix and its inverse into cache 
## If the matrix is reused to find inverse cache is checked first
## if inverse matrix is found in the cache, inverse is not 
##re-calculated and the cached inverse matrix is used instead. 


## This function takes a matrix as input. Stores the matrix in a 
##cached varaible. creats the Set and get function that sets and 
#retrieves this cached value of input matrix. This functions
## also sets and gets the cached values of inverse of the input 
##matrix.
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set_inverse <- function(inv) m <<- inv
      get_inverse <- function() m
      list(set = set, get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


##This function calculates the inverse of an invertible matrix
##created through the makeCacheMatrix function if it has not 
##already been calculated

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      m<- x$get_inverse()
      if (!is.null(m)){
            return (m)
      }
      data<-x$get()
      m1<-solve(data)
      x$set_inverse(m1)
     m1
      
}
