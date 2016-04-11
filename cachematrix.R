#Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly.
#Target here is to write a pair of functions that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# i. set the value of the matrix
# ii. get the value of the matrix
# iii. set the value of inverse of the matrix
# iv. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
	#Encapsulate in to a list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The function cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips
# the computation. If not, it computes the inverse, sets the value in the cache
# via setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

#> Usage example:
#> x <- matrix(1:4, nrow=2, ncol=2)
#> m <- makeCacheMatrix(x)
#First invoke
#> s1 <- cacheSolve(m)
#> print(s)
#    [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#Second invoke to test for caching
#> s2 <- cacheSolve(m)
#getting cached data.
#message shown for caching
#> print(s2)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#result is same
