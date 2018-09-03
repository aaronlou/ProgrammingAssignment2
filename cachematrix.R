## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    revM <- NULL
    set <- function(y){
        x <<- y
        revM <<- NULL
    }
    get <- function() x
    setRevM <- function(solve) revM <<- solve
    getRevM <- function() revM
    list(set=set,get=get,setRevM=setRevM,getRevM=getRevM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    revM <- x$getRevM()
    if(class(revM) == "matrix") {
        message("getting cached data")
        return(revM)
    }
    data <- x$get()
    revM <- solve(data, ...)
    x$setRevM(revM)
    revM
}
