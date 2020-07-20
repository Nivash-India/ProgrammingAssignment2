## Caching the inverse of a matrix
## Created Function will store a matrix and caches its inverse
## Essentially function will create a matrix object to cache its inverse 
## makeCacheMatrix is a function that returns a list of functions, Its puspose is to store a martix and a cached value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
                }
        get<-function()x
        setInverse <- function(inverse) i<<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

}


## This function computes the inverse of the "matrix" which is created from makecacheMatrix. Once the inverse is already calculated ( also no changes in matrix), 
## then we can retrive the inverse from cache
## cacheSolve- Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
