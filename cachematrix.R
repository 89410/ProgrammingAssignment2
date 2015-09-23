# makeCacheMatrix Caches an inverse matrix created by cacheSove.R
# Stores 4 functions set_matx, get_matx, get_inv_matx and 
# set_inv_matx in a list

makeCacheMatrix<- function( x = matrix()) {
  m <- NULL                        # Initialize "m" 
   set_matx <- function(y) {       # allows "x" and "m" of the main function
   x <<- y                         # to be changed
   m <<- NULL
   }
  
  get_matx <- function() x         # retrieves 'x'   
  set_inv_matx <-function(solve) m <<-solve   # Caches inverse matrix from 
                                              # cacheSolve.R       
  get_inv_matx <- function () m               # Retirieves inverse matrix
  list(set_matx = set_matx, get_matx = get_matx, set_inv_matx = set_inv_matx, 
       get_inv_matx = get_inv_matx)           # List containinig 4 functions 
}


## Takes the matrix inputted to makeCacheMatrix.R, 
# generates the inverse matrix and returns it to
# makeCacheMatrix.R to be cached

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
m <- x$get_inv_matx()         # Pulls inverse matrix from makeCacheMatrix  
  if(!is.null(m)){              # Checks if inverse matrix already exists
    message("getting cached data") # Tells user if it is using a cached matrix
    return(m)
  }
  data <- x$get_matx()          # Generates the inverse matrix 
  m <- solve(data,...)
  x$set_inv_matx(m)             # Sends inverted matrix to makeCacheMatrix
  m                             # to be cached

}
