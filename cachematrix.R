## This function aim to get the inverse of matrix.
## First function trys to get the original matrix. 
## Second function trys to get the inverse matrix 
## and return the inverse matrix. 

## "makeCacheMatrix" can input the matrix
## , call the function "cacheSolve"
## and create a data list for "cacheSolve".
## The function "cacheSolve" get the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y){
               x <<- y
               m <<- NULL
       }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
       list(set = set, get = get,
            setmatrix = setmatrix,
            getmatrix = getmatrix)
}
## To cahche a inverse and return answer.
## if the inverse has been calculated,
## just gets from cached data(list).
## if not,just calculate it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        ##if(!is.null(m)&& m == m) make sure 
        ##the matrix is the same as you want
        if(!is.null(m)&& m == m) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

