## This file already contained the stub 1.makeCacheMatrix and 2.cacheSolve.
## Assignment is to implement the pair of functions such that it can cache the inverse of a matrix

## Most of the comments in this file is thanks due to a clear explanation and comments by Bill Hilton 
## in a forum discussion thread Making sense of Assignment 2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## We are taking a function and converting it into an R object (class more specifically). 

## This object is a container which holds the created matrix and the variable MI which is initialized to NULL. 

makeCacheMatrix <- function(x = matrix()) {   ## Calling makeCacheMatrix will create an object of type matrix, 
                                              ##with variable MI initialized to NULL.  
  MI<- NULL
  
  set<- function(y){  # takes an input matrix
    x<<- y            # saves the input matrix
    MI<<- NULL        # resets the mean to NULL.
  }
  
  get<- function(){x}  # this function returns the value of the original matrix
  
  setinverse<- function(matinverse){MI<<- matinverse} # this is called by cacheSolve() during the first cacheSolve() access 
                                                      #and it will store the value using superassignment
  
  getinverse<- function(){MI}             # this will return the cached value to cacheSolve() on subsequent accesses

  
  list(set= set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## The second function, cacheSolve(x) When called, first gets the inverse from the object x (x$getinverse()). 
## If there is a non-NULL value in the inverse then a message is sent and the inverse is returned and the cacheSolve function stops.

## If the fetched inverse value is NULL then the if() statement is skipped and the next four lines are executed, 
## namely a fetch of the original matrix (x$get()), calculation of the inverse of that matrix, 
## storing this inverse value back in the object (x$setinverse(MI)) and the return of MI to end the function.


cacheSolve <- function(x, ...) {  # the input x is an object created by makeCacheMatrix
        
  MI<- x$getinverse()     # accesses the object 'x' and gets the inverse of the matrix
  if(!is.null(MI)){       # if inverse was already cached (not NULL)...
    message("getting cached data")  # ... send this message to the console
    return(MI)            # ... and return the inverse ... "return" ends 
  }
  dm<- x$get()            # we reach this code only if x$getinverse() returned NULL
  MI<- solve(dm, ...)     # if MI was NULL then we have to calculate the inverse
  x$setinverse(MI)        # store the inverse matrix in x
  MI                      # return the inverse matrix to the code that called this function
}
