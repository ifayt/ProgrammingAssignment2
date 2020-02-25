## This a pair of functions that cache the inverse of a matrix.


##  First function creates a special "matrix" object that can cache its inverse.
# It is a list containing 4 functions: 
# 1 to set the matrix
# 2 to get the matrix
# 3 to set the inverse of the matrix
# 4 to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ # "set" is a function that changes the matrix stored in the main function
    x <<- y           # this line replaces the matrix x with y (the input) in the function makeCacheMatrix
    inv <<- NULL      # the value of the inverse of the matrix is restored to NULL, as the old inverse does not apply anymore
  }
  get <- function() x  # function to return the matrix stored in the main function (makeCacheMatrix)
  setinv <- function(solve) inv <<- solve   #function to store the value of the input in a variable inv into the function makeCacheMatrix (does not have to be the actual inverse of the matrix) 
  getinv <- function() inv                  #function to return the value stored in the variable inv (is not necesssarily an inverse value of the matrix)
  list(set = set, get = get,                #this stores the 4 above functions in a list. In order to access each function from the list $ operator should be used.
       setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
#retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Firstly, it is verified whether the inverse 
  # value of the matrix exists and is stored in 
  # the getinv function from the makeCacheMatrix function
  inv <- x$getinv()   #we access the value stored in getinv            
  if(!is.null(inv)){  #If it is non-Null this value is returned and the function stops here. 
    message("getting cached data")
    return(inv)
  }
  #Otherwise the inverse of the Matrix is calculated using
  # the function "solve" on the matrix stored with makeCacheMatrix
  data <- x$get()             #use as an input data the matrix stored in get in makeCacheMatrix
  inv <- solve(data,...)    #use the solve function calculating the inverse of the matrix on the data
  x$setinv(inv)           # store the calculated inverse value of the matrix in the object generated assigned with makeCacheMatrix
  inv
}

###################################################################################
#TEST THE FUNCTIONS
###################################################################################

#create a matrix
matr <- matrix(rnorm(100), nrow =5, ncol = 5)
#check that its determinant is non-zero and is invertible
det(matr)

#use makeCacheMatrix function on the matrix above
a <- makeCacheMatrix(matr)

#use cacheSolve function
cacheSolve(a)

#compare the result with the inverse of the initial matrix  matr
solve(matr)

#checking caching part of the function
a$setinv(matrix(1:25, nrow = 5, ncol =5))
cacheSolve(a)
