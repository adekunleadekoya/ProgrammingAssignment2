## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  makeCacheMatrix is a function that creates a special "matrix", which is
##  really a list containing a function to

##	1.  set the value of the matrix
##	2.  get the value of the matrix
##	3.  set the value of the inverted matrix
##	4.  get the value of the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
		 m <- NULL  ## m is the inverse of x
             set <- function(y) {   # y is a matrix passed as a parameter
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(invertedMatrix) m <<- invertedMatrix
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse ,
                 getinverse = getinverse )

}


## Write a short comment describing this function

##	cacheSolve is a function that calculates the inverse of a special "matrix"
##	created with the above function. However, it first checks to see if the
##	inverse has already been calculated. If so, it `get`s the inverse from the
##	cache and skips the computation. Otherwise, it calculates the inverse of
##	the given matrix and sets the value of the inverse in the cache via the `setinverse`
##	function.

cacheSolve <- function(x, ...) {
       

            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached inverse m of a special matrix x")
                    return(m)
            }
            data <- x$get() # gets the special matrix 
            m <- solve(data)  # computes the inverse m of the special matrix
            x$setinverse(m)   # sets the inverse m of special matrix x
            m  ## Return a matrix that is the inverse of 'x'

}

##   m<-matrix(1:4,2,2)
##   solve(m)      (*)

##    x=makeCacheMatrix(m)
##	cacheSolve(x)  (**)

##	(*)  and (**) gave me the same result when the makeCacheMatrix and
##	 cacheSolve were invoked as 
