## cache the inverse S of a matrix in order to avoid recomputing 

#test cases
# Create a matrix x
#x <- matrix( c(2, 4, 3, 1), nrow=2, ncol=2)
# assign matrix
#t1x <- makeCacheMatrix(x) 
#Return the matrix
#t1x$get() 
#Return the inverse
#cacheSolve(t1x) 
#Call  2nd time, so return cached inverse
#cacheSolve(t1x) 

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix

# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(mx= matrix()) {
    
    #initialize inverse cache variable
    invmx <- NULL;
    
    # Set value of matrix to mx
    set <- function(matrix ) {
        mx <<- matrix 
        invmx <- null;
    }
    
    # Get  value of matrix
    get <- function() mx
    
    ##  set  inverse of matrix to cache var
    setInverse <- function(inverse) {
        invmx <<- inverse
    }
    ## get cached inverse
    getInverse <- function() {
        ## Return the inverse property
        invmx
    }
    
    
    # Return the matrix with our newly defined functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve: get cached matrix invmx, If invmx not avaialable
cacheSolve <- function(x, ...) {
    invmx <- x$getInverse()
    # If the inverse is already available, return it
    if (!is.null(invmx)) {
        message("get cached inverse")
        return(invmx)
    }
    # The inverse is not yet cached, so  calculate & cache
    data <- x$get()
    invmx <- solve(data, ...)
    # Cache the inverse
    x$setInverse(invmx)
    # Return it
    invmx
}



