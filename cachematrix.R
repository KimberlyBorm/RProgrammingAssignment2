## Put comments here that give an overall description of what your
## functions do

#These two functions work together to set up a management system for storing a recalling cached matrices to avoid repeat calculations. 
#The first function creates the storage image and the second calculates the inverse and stores it in the image, if it has not been already calculated previously. 

## Write a short comment describing this function

# This function creates a structure to manage a matrix(x) and it's inverse(m). 
#Allowing the user to set and get the matrix and 'store a recomputed inverse' (cached) using setinverse().
#This does not compute the inverse, but is reusable and efficient for recalling it however it is computed 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes an inverse matrix of the object matrix stored in makeCacheMartix, then caches it and returns the results.  
#If the inverse has already been calculated and cached it uses the cached results to avoid unnecessary calculations


cacheSolve <- function(x, ...) {
                                    ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}
