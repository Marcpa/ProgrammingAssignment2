## These functions can cache the inverse of a matrix

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         i <- NULL 
         # When the set function is called, Inverse is set to NULL and assigns the argument y to x 
         set <- function(y){ 
                 x <<- y
                 i <<- NULL  
         } 
         get <- function() x # get function returns the matrix 
          
         setInverse <- function(solve) i <<- solve # setInverse overrides i and assigns the argument 
          
         getInverse <- function() i # getInverse returns the Inverse 
        
         # creates a list of the functions
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)          
 } 



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix function
cacheSolve <- function(x, ...) { 
         i <- x$getInverse() # Retrives the most recent value for the inverse 
          
         if(!is.null(i)){ 
                 message("getting cached Matrix") 
                 return(i) 
                 # If the value of Inverse is NOT null, cacheSolve returns that value         
         } 
         # If Inverse is NULL, then retrieve matrix and calculate inverse with solve() function 
         message("calculating Inverse Matrix") 
         data <- x$get() 
         i <- solve(data, ...) 
         x$setInverse(i) # Sets Inverse to new value    
         i ## Return a matrix that is the inverse of 'x'
 } 