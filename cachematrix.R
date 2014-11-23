## Following two functions allow to create special kind of matrix which caches its inverse

## Creates a special "matrix" object that can cache its inverse. To get inverse use cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
	inverted <- NULL

    set <- function(y) {
            if(!identical(x, y)) {
            	inverted <<- NULL            	
            	x <<- y
            }
    }
    get <- function() x
    set_inverse <- function(inv) inverted <<- inv
    get_inverse <- function() inverted
    list(set = set, get = get, 
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {        
        inv <- x$get_inverse()

        if(is.null(inv)) {
                ma <- x$get()
                inv <- solve(ma, ...)
                x$set_inverse(inv)
                return(inv)
        }
        else {
        	message("getting cached inverted matrix")
        	inv
        }
}
