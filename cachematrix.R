# Example
makeVector <- function(x = numeric()) {
  # sets x equal to an empty numeric vector
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean      , getmean = getmean)
  
}
# This function creates a special "vector", which is really a list of:
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the mean
#4. get the value of the mean

cachemean <- function(x, ...) {
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

# Practice
a <- makeVector()       # initialize
a                       # shows that a is a list of functions
class(a)                # a is a list
class(a$set)            # set (within a) is a function
a$set(c(1, 2, 3, 4, 5, 6, 7)) # set the function
a$get()                 # returns the vector that was previously "set"
cachemean(a)
cachemean(a)


tf <- matrix(1:4, 2, 2)         # Creates a matrix
tf
tf1 <- solve(tf)                # solve(tf) calculates the inverse of the matrix tf
tf1
tf %*% tf1                      # %*% is matrix multiplication


# Part 1: makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix
  I <- NULL
  # Set the inverse equal to NULL
  
  set <- function(y){
    x <<- y
    # set function assigns the argument to x
    I <<- NULL
    # Once the set function is called, Inverse is re-set to NULL
  }
  get <- function() x
  # get function returns the matrix
  
  setInverse <- function(solve) I <<- solve
  # setInverse overrides the previous value of I and assigns the argument to Inverse
  
  getInverse <- function() I
  # getInverse returns the Inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # creates a list of the functions
  
}

# Part 2: cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  # Retrieves the most recent value for the inverse
  
  if(!is.null(I)){
    message("getting cached data")
    return(I)
    # If the value of Inverse is NOT null, cacheSolve returns that value        
  }
  # If the value of Inverse is NULL, then you retrieve matrix x and calculate the inverse with the solve() function
  message("newly calculating data")
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  # Sets Inverse to the newly calculated value   
  I #Returns the new Inverse value
}
