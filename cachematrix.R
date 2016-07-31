## This pair of functions work together to enable the caching of the calculating of the inverst of a matrix.
## First makeCacheMatrix creates a special type of matrix which caches it's inverse.  cacheSolve can be run to check if the inverse
## calculation has already been cached.  If so, it returns the cached inverse.  If not, it calculates the inverse and updates the
## cached inverse in the special makeCacheMatrix matrix.
## 
## Assumptions:  These functions assumes that the matrix

## 
## Attributions:  These two functions represent just minor modifcations of the makeVector and cachemean functions created by R. Peng 
##                for the "Coursera Programming in R Course" Assignment 2.
##                The descriptions of what these functions do is largely based on the assignment text written by R.Peng
##                The coding of this assignment was made possilbe thanks to the excellend explanation of the makeVector and cachemean 
##                functions posted by Leonard Greski on the disucssion board for this assignment



## makeCacheMatrix is a function which creates a special matrix which caxhes it's onw inverse.
## This funciton establishes "getters" and "setters" to access both the matrix itself and its inverse

makeCacheMatrix <- function(x = matrix()) {   

      i <- NULL                                   #initialize the inverse to NULL to start
  
      set <- function(y = matrix()) {             #function for setting a new matrix
        x <<- y
        i <<- NULL
      }
  
      get <- function() x                         #function for returning the matrix
      setInv <- function(inv) i <<- inv           #function for setting the inverse
      getInv <- function() i                      #function to retrieve the cached inverse
      list(set = set,                             #the list of references to the get/set functions is returned
           get = get,
           setInv = setInv,
           getInv = getInv)

}


## cacheSolve is function which returns the inverse of 'x'
## This function can be used on the 'special' matrices created using the makeCacheMatrix funciton above.
## This function first looks to see if the inverse has already been calculated by the makeCacheMatrix function.
## If so, and the matrix hasn't changed, it is retrieved.
## The x passed to this function is acctually a list of setters and getters into the matrix created by makeCacheMatrix.
## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      i <- x$getInv()                         #Attempt to retrieve the cached inverse
      
      if(!is.null(i)) {                       #If the inverse exists (it's not NULL), retrieve it
        message("getting cached inverse")
        return(i)
      }
 
      tempMatrix <- x$get()                   #if the inverse doesn't already exist, we need to calculate it
      i <- solve(tempMatrix,,,,)
      x$setInv(i)
      i
}
