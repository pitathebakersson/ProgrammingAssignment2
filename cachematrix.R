## Put comments here that give an overall description of what your
## functions do

## This function returns a list of functions that set the matrix, get
##the matrix, set the inverse of the matrix and get the inverse of the matrix
##this function also stores the inverse of the matrix x in inverseM

makeCacheMatrix <- function(x = matrix()) {
    
    inverseM <- NULL
    set <- function(y)
    {
        x<<-y
        inverseM <<- NULL
    }
    get<- function() x
    setInverse<-function(inverse) inverseM <- inverse
    
    getInverse <- function() inverseM
    
    list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
    

}


## This function returns the inverse matrix. If the inverse has been computed
##already, it will return the cached version. If not, this will compute
##the inverse and then return the computed value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM<- x$getInverse()
        if (!is.null(inverseM))
        {
            print("Getting cached data.")
            return(inverseM)
        }
        data <- x$get()
        inverseM <- solve(data,...)
        x$setInverse(inverseM)
        inverseM
}
