## The following 2 functions enable R to cache an Inverse matrix calculation. This is usfull to save computetional
## time in case that the same matrix need to be Inverse several times.


## The function makeCacheMatrix creates a special list (vector) containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the solved matrix
## 4.get the value of the solved matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {   ## this function get the matrix and save in "static varibale" and initialize m to NULL
                x <<- y
                m <<- NULL
        }
        get <- function() x    ## this function get the matrix
        setsolve <- function(matrix) m <<- matrix  ## This function set the inverse matrix in cache
        getsolve <- function() m                   ## This function get the inverse matrix from cache
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## The following function calculates the inverse matrix of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()     ## Bring results from cache
        if(!is.null(m)) {     ## Exist in cache no need for calculation
                message("getting cached data")
                return(m)
        }
        data <- x$get()       ## If 1st time get the data and slove the matrix
        m <- solve(data, ...)
        x$setsolve(m)         ## Save results in cache
        m        
}


## Example of how to use it:

## a <- makeCacheMatrix() #initialize vector a
## a$set(matrix(1:4,2,2)) # set the matrix
## a$get() #get the matrix 
## cacheSolve(a) #calculate the mean 
## cacheSolve(a) #when is called back use the cached mean 
