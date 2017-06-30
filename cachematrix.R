## These following functions are useful to calculate and show the inverse matrix
## of a given matrix.


## This function set a value for the matrix to elaborate and get the matrix value
## to the operator

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function calculate the inverse matrix of the matrix x.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

        ## Example


x<-matrix(1:4,2,2)

m<-makeMatrix(x)
m$get()
cacheinv(m)
