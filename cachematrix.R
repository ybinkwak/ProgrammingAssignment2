## Generate the list of invertible matrix and its inverse matrix, and 
## prevent the re-computation for calculating inverse matrix.
## Cache matrix constructor only contains square matrix, which is generally invertible. 

## Construct the cache matrix and its inverse of given invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        if (!is.matrix(x) || nrow(x) != ncol(x)) {                
                message("This is not a square matrix.")                
                x <- matrix()                        
        }
        set <- function(y){
                if (!is.matrix(y) || nrow(y) == ncol(y)){
                        x <<- y
                        inv <<- NULL
                } else {
                        message("This is not a square matrix :(")
                        return(x)
                }
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculate the inverse matrix of Cache Matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached date")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv                
}