## The cachematrix.R file contains two functions, makeCacheMatrix() and 
## cacheSolve(). The first function in the file creates an R object 
## that stores a matrix and its inverse. 
## The second function requires an argument that is returned by 
## makeCacheMatrix() in order to retrieve the inverse from the 
## cached matrix that is stored in the makeCacheMatrix() object's 
## environment.

## makeCacheMatrix
## set the value of x and NULL to the value of i - so whenever x is reset the value of 
## i cached in the memory of the object is cleared, forcing subsequent 
## calls to cacheSolve() to recalculate the inverse rather than retrieving 
## the wrong value from cache.

## getter for the matrix (x) defined
## setter for the inverse (i) defined


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setinverse <- function(solve) i <<- solve ## the <<- assignment operator to assign the input argument to the value of i in the parent environment
        getinverse <- function() i
        
        list(set = set,                ## gives the name 'set' to the set() function defined above
             get = get,                ## gives the name 'get' to the get() function defined above
             setinverse = setinverse,  ## gives the name 'setinverse' to the setinverse() function defined above
             getinverse = getinverse)  ## gives the name 'getinverse' to the getinverse() function defined above
        
        
}

## cacheSolve
## cacheSolve() is the only place where the solve() function is executed
## the makeCacheMatrix would be incomplete without cacheSolve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse() ##calling getinverse() on the input object
        if(!is.null(i)) {
                message("getting cached data")
                return(i) 
                
## if not NULL cached i returned to parent environment
## if !is.null(i) is FALSE, cachemean() gets the matrix from the input object, 
## calculates a solve(), uses the setinverse() function on the input object to 
## set the inverse in the input object, and then returns the value of the inverse 
## to the parent environment by printing the inverted object.
                
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i) ## set the inverse in the input object, and then returns the value of the inverse to the parent environment by printing the inverted object.
        i
}
