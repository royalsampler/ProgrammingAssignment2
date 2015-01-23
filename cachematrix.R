## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
# The pair of functions below are useful in creating an object that stores both the matrix and a cache of its inverse

# assume that the matrix supplied is always invertible

## MakeCacheMatrix is the function that constructs our CacheMatrix object. It creates the object and defines its four member functions.
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL   #inversed is the cached inverse of the matrix. it should be set to null (not yet calculated) on instantiation
  #note where x and inversed are scoped
  
  #the set function allows you to store the matrix itself within the CacheMatrix
  set <- function(y) {
    x <<- y               #store the matrix (passed in as y) to the variable x
    inversed <<- NULL     #set inversed to NULL - do not calculate the inverse unless explicitly told to do so
  }
    
  get <- function() x # the get function returns the matrix
  
  setinverse <- function(inverse) inversed <<- inverse #setinverse allows you to explicitly set the cached inverse
  
  getinverse <- function() inversed #getinverse simply returns the cached inverse
  
  #finally, return the CacheMatrix as the output of the function. 
  #behind the scenes, the CacheMatrix is actually a list object containing the object's member functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of x by first checking if the inverse of the matrix has been cached, if not then solves for it. 

cacheSolve <- function(x, ...) {
       
  inverse <- x$getinverse() #first try to get the cached value for the inverse of the matrix
  if(!is.null(inverse)) {   #now check to see if there was a non-null value there
    message("getting cached data") #if we got something back, just return it, no need to calculate 
    return(inverse)
  }
  
  #if the cached value came back null, we need to solve for it ourselves.
  message("number crunching in progress") #warn the user that we are about to solve the inverse of the matrix
   inverse <- solve(x$get(), ...) #use solve without a "b" parameter to calculate the inverse of the matrix
  x$setinverse(inverse) #store the solved value back to the cache for next time
  inverse #return the inverse
}


