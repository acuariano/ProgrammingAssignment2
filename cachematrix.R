## To avoid recomputing the inverse of a matrix each time is needed
## we are implementing cache. Our implementation has two functions:
## makeCacheMatrix - creates a cache for our matrix parameter.
## cacheSolve - uses makeCacheMatrix to cache and calculate inverse.
## Use makeCacheMatrix to create a cacheable version of the input
## matrix and pass the result to cacheSolve whenever you want the 
## inverse.


## makeCacheMatrix creates a cache for our matrix parameter, which 
## stores the original parameter using $get and $set; and the 
## inverse using $getinverse and $setinverse. Notice that the 
## semantic is external: whatever you set with $setinverse will be
## retrieved with $getinverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The paremeter to "cacheSolve should be an object created by
## makeCacheMatrix function. "cacheSolve" uses the $getinverse
## to get and return the inverse of the matrix x. If it's null
## it calculates the inverse and stores it using the #setinverse
## before returning it.
## Note: the name "cacheSolve" seems to indicate that this is
## a cached version of "solve", but that's not the case.
## In particular, if you pass a second paramter to cacheSolve
## it will be ignored and the inverse of x will be returned.
## We are only interested in inversion.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
