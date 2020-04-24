## The below functions fulfill the requirements for Week 2/lexical scoping.

## This function creates a matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## This function returns the cache of the above function.

cacheSolve <- function(x, ...)
{
        inv <- x$getInverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
