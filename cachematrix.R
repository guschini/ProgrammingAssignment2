#---------------------------------
# Gets a matrix, assumed square and invertible
#
# Returns a list of functions for working
# with with this matrix
#
# The functions set and get
# 1. the matrix
# 2. the inverse of that matrix
#---------------------------------
makeCacheMatrix <- function(x = matrix()) {
  x_inversed <- NULL
  
  set <- function(y) {
    x <<- y
    x_inversed <<- NULL
  }
  
  get <- function() x
  
  set_inversed <- function(i){
    x_inversed <<- i
    message("data have been cached")
  }
  
  get_inversed <- function() x_inversed
  
  #return a list of the functions declared above
  list(set          = set,
       get          = get,
       set_inversed = set_inversed,
       get_inversed = get_inversed)
  
}

#---------------------------------
# Gets a result of makeCacheMatrix(x) function,
# a matrix provided with functions to work with it
# 
# Returns the inverse of the matrix
#
# If the same matrix has been already passed, returns
# its cache result
#---------------------------------
cacheSolve <- function(x, ...) {
  already_inversed <- x$get_inversed()
  if(!is.null(already_inversed)) {
    message("getting cached data")
    return(already_inversed)
  }
  else {
    message("no cached data found")
  }
  data <- x$get()
  just_inversed <- solve(data, ...)
  x$set_inversed(just_inversed)
  ## Return a matrix that is the inverse of 'x'
  just_inversed
}

############# USAGE ##############
#
# In Linux-like console
#
# cd ~/
# git clone https://github.com/guschini/ProgrammingAssignment2.git
#
# In R environment
#
# setwd("~/ProgrammingAssignment2/")
# source("cachematrix.R")
# m <- matrix(1:4, 2, 2)
# mf <- makeCacheMatrix(m)
# inversed_m <- cacheSolve(mf) # computes the inverse, caches it, and returns then
# inversed_m <- cacheSolve(mf) # second time, the result is taken from the cache
#
##################################