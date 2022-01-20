## Put comments here that give an overall description of what your
## functions do

## This function make a special matrix which cache its inverse by asigning the NULL value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } ## set the value of the matrix
        get <- function() x ## get the value of the matrix
        setinv <- function(solve) m <<- solve ## set the inverse of the matrix
        getinv <- function() m ## get the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This function which works in tandem with makecachematrix is able to release the  cached inverse matrix

cacheSolve<-function(x,...){
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } ## check if the inverse is already in the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m) ## set the inverse if it is not in the cache
        m
}

## example : 
a<-matrix(c(1,0,0,1,2,0,3,0,1),ncol=3,nrow=3)
b<-makeCacheMatrix(a)
b$get()## the marix is set
b$getinv() ## the inverse is cached 
cacheSolve(b) ## gets cached data
b$getinv()
