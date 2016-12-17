##to make a list which store the original matrix and the inverse of the matrix
##make a storage list

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(k) inv <<- k
        getinv <- function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## make the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()         ##create the matrix which is ready to be inversed
        inv <- solve(mat,...)  ##crete the inverse of the martrix
        x$setinv(inv)
        inv
}
