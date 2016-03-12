# Programming Assignment 2 : Write an R function that is 
# able to cache potentially time-consuming computations.

# create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #create empty matrix to store inverted matrix
        i <- NULL
        
        #pass the value of matrix
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        #get the value of matrix
        get <- function() {
                x
        }
        
        #pass the inverse of matrix
        setInverse <- function(solve){
                i <<- solve
        }
        
        #get inverse of matrix
        getInverse <- function(){
                i
        }
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Return inverse of matrix returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {

        #check if inverse of matrix has been calculated
        i <- x$getInverse()
        
        #if inverse is pre-calculated, get inverse from cache 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #else, calculate and return the inverse of 'x'
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}