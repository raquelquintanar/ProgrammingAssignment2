## This pair of functions caches the inverse of a matrix


##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(matriz = matrix()) {
        inv <- NULL
        set <- function(y) {
                matriz <<- y
                inv <<- NULL
        }
        get <- function() matriz
        setinv <- function(inverso) inv <<- inverso
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(matriz, ...) {
        inv <- matriz$getinv()
        if(!is.null(inv)) {
                message("getting cached solved matrix")
                return(inv)
        }
        data <- matriz$get()
        inv <- solve(data, ...)
        matriz$setinv(inv)
        inv
}
