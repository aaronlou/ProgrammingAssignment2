## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #initialize a matrix
    revM <- NULL
    
    #set is used to give a specified input value
    set <- function(y){
        x <<- y
        revM <<- NULL
    }
    
    #return the matrix which we want to solove
    get <- function() x
    
    #we could also directly give a inversed matrix
    setRevM <- function(solve) revM <<- solve
    
    #return  the inversed matrix
    getRevM <- function() revM
    list(set=set,get=get,setRevM=setRevM,getRevM=getRevM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # test if the inversed matrix of input x does already exist
    revM <- x$getRevM()
    if(!is.null(revM)) {
        message("getting cached data")
        #if we really have, just return it, and this function stop here
        return(revM)
    }
    
    #if not, we need to solve it manually
    data <- x$get()
    revM <- solve(data, ...)
    x$setRevM(revM)
    revM
}
