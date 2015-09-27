## This file defines two main functions, makeCacheMatrix() and cacheSolve(). 
## Their aim is to cache matrix inversion computation, a time-consuming task. 
##
## makeCacheMatrix() : creates a special "matrix" object (list of functions)
##
## cacheSolve() : computes the inverse of the object created by makeCacheMatrix
##                or retrieves it from the cache


## Creates a special object (list of 4 functions) from an input matrix x: 
##
##      set()        : sets the value of the matrix
##      get()        : gets the value of the matrix
##      setinverse() : sets the value of the inverse
##      getinverse() : gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

        # initializes variable i that will store x matrix inverse
        i <- NULL
        
        # changes the matrix x stored in the main function to y and cleans
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # returns the matrix x stored in the main function
        get <- function() x

        # stores the value of the input in a variable i in the main function         
        setinverse <- function(inverse) i <<- inverse
        
        # returns the value of variable i from the main function
        getinverse <- function() i
        
        # main function output
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Computes the inverse of the special "matrix" object x returned by 
## makeCacheMatrix(). If the inverse has already been calculated, cacheSolve() 
## retrieves the inverse from the cache.
cacheSolve <- function(x) {
        
        # if there is cached data, retrieve it and return from cacheSolve()
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        # if there is no cached data, calculate the inverse
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
