## Put comments here that give an overall description of what your
## functions do

## Test values
#mat <- as.matrix(replicate(3, rnorm(3)))
#mat2 <- as.matrix(replicate(2, rnorm(2))) 
#solve(mat)
#solve(mat2)

## Write a short comment describing this function
# makeCacheMatrix creates and object "special matrix", that is
# actually a list that contains 4 functions. The purpose of this
# object is to store the inverse matrix 
# The purpose of the functions inside the main function are:
# set(): change the input matrix of the special matrix 
# (instead of creating a new special matrix object)
# get(): let the cacheSolve function access input matrix
# setinverse(): after the cacheSolve function calculated the inverse
# it gives it back to makeCacheMatrix and stores the inverted matrix
# getinverse(): gives the stored inverse matrix to the cacheSolve.
# if there isn't any stored matrix, then NULL is returned

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { x <<- y; m <<- NULL }
  get <- function() { x }   
  setinverse <- function(solve) { m <<- solve }  
  getinverse <- function() { m } 
  list(get = get,              
       setinverse = setinverse,  
       getinverse = getinverse,
       set = set)   
}

## Write a short comment describing this function
# This function takes an an input a special matrix list
# Then if askes the special matrix if it has a stored value for the inverse of the 
# input matrix through getinverse(). If it has a stored value then it returns 
# that value together with a message "getting cached data".
# If the special matrix doesn't have a stored value then it will
# take the input matrix (through get()) and calculate the inverse
# and then give it back to the special matrix to store through setinverse()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() 
  if(!is.null(m)) {         
    message("getting cached data")  
    return(m)                      
  }
  data <- x$get()        
  m <- solve(data, ...)  
  x$setinverse(m)          
  m         
}

## One thing I didn't understand was why I had to include 
# the set function in the list returned by makeCacheMatrix
# In the Cacheman_vextor.R the set function is not included
# but I couldn't get it to work without including it!


## Test arguments
#a <- makeCacheMatrix(mat)
#a$set(mat2)
#cacheSolve(a)
#b <- makeCacheMatrix(mat2)
#cacheSolve(b)
