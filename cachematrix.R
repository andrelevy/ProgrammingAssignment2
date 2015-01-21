## The two functions, makeCacheMatrix() and cacheSolve(), work together
## by calculating the inverse of an inversible square matrix and placing
## the inverse in cache, so that it available in the Global Environment
## and need not be repeatedly computed.

## makeCacheMatrix() takes a square invertible matrix as argument and
## creates a  list of functions that operate on the matrix,
## using function 'solve()'. [ginv()  calculates a Moore-Penrose Generalized 
## Inverse of a matrix, and requires loading the MASS package.]
# The list of functions returned include
# set the matrix
# get the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL    #initialized inv
  
  # set the value of the matrix
  set <- function(y) {     
    x <<- y     # searches environment where x is defined and assigns y to x in that environment
    inv <<- NULL
  }
  
  # get the value of the vector
  get <- function() x   #function get(x) will give you x, the matrix
  
  # set the value of the mean
  setinv <- function(inverse) inv <<- inverse 
  
  ## Because we use the operator '<<-' function setinv(x) searches up from enclosing environment (of the setinv function) 
  ## for the name (inv), which is in this case the makeCacheMatrix environment. 
  ## It finds it at the top of the function {inv <- NULL}. If it wasn't there, the <<- operator in setmean wouldn't find it in 
  ## makeCacheMatrix's environment, and thus would end up defining inv in the global environment.
  
  # get the value of the mean
  getinv <- function() inv         #function getinv(x) will return solve(x)
  
  list(set = set, get = get,       #returns 4 functions
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes as an argument the function list produced by makeCacheMatrix
## checks to see if the inverse has already been attributed to variable 'inv'
## and if not, attributes the inverse to that variable

cacheSolve <- function(x, ...) {  
  
  # Test whether 'inv' already has a value attributed
  inv <- x$getinv()
  
  # In case has, ie is not NULL, then indicate cached value is being retrieved, and return cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise get the matrix 'x' and place it temporarily in variable 'data', using the get function defined above
  data <- x$get()
  
  # calculate the inverse of the matrix using solve and store inverse matrix in variable 'inv'
  inv <- solve(data, ...)
  
  # use setinv function to cache the inverse in variable 'inv'
  x$setinv(inv)
  
  # print the inverse of the matrix
  inv
}

## Example
# > x <- c(4,3)
# > y <- c(3,4)
# > z <- cbind(x,y)
# > myMatrix<-makeCacheMatrix(z)
# > myMatrix<-makeCacheMatrix(z)
# > cacheSolve(myMatrix)
# [,1]       [,2]
# x  0.5714286 -0.4285714
# y -0.4285714  0.5714286
# > cacheSolve(myMatrix)
# getting cached data
# [,1]       [,2]
# x  0.5714286 -0.4285714
# y -0.4285714  0.5714286
