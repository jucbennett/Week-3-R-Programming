##Julia Catherine Bennett, R Programming Assignment 2, 27 April 2018

## Put comments here that give an overall description of what your
## functions do
        ## makeCacheMatrix takes a matrix that can be inversed and caches it
        ## cacheSolve retrieves the cached matrix (if already cached) or calculates the inverse and caches it through makeCacheMatrix

## Write a short comment describing this function

        #makeCacheMatrix first takes a matrix & stores it (set) in the parent environment and also stores a null value in the parent
        ## environment which will become the cached inverse

        ##Next, makeCacheMatrix will store the inverse (calculated in cacheSolve) in the parent environment (setinverse)

        ##(getinverse) retreives the cached inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

        ##cacheSolve first retrieves the cached inverse if it exists (is not still null)
        ##If the cached inverse has not been calculated & stored (is still null), cacheSolve retrieves the matrix (x$get) and 
        ##solves for the inverse
        ##Then, cacheSolve uses x$setinverse() to set the inverse to the cached value in makeCacheMatrix and finally displays the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <<- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <<- solve(data, ...)
        x$setinverse(m)
        m
}

