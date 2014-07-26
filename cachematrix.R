# This function will create a default 2x2 matrix with values 1 through 4.
# The user can of course override this default matrix.  In this function we:
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse


makeCacheMatrix <- function(x = matrix(1:4,2,2)) # create a 2x2 square matrix
{  
    is_inv <- NULL                               # set inverse flag to NULL
    set <- function(y)
    {
        x <<- y
        is_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) 
    is_inv <<- inverse
    getinverse <- function() is_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function creates the inverse of the matrix created in the
# function makeCacheMatrix. It checks to see if the inverse has already been
# created.  If so, it returns the information from cache to save compute cycles.
# Otherwise, it creates the inverse and stores it in cache.

cacheSolve <- function(x, ...) 
{
    is_inv <- x$getinverse()
    if(!is.null(is_inv))
    {
        message("getting the data from cache.")
        return(is_inv)
    }
    data <- x$get()
    is_inv <- solve(data, ...)
    x$setinverse(is_inv)
    is_inv
}