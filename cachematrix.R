## These functions use the <<- operator to assign the value to an object
## in a different environment

## makeCacheMatrix sets the value of the matrix, 
## gets the value of the matrix, sets the inverse of the matrix,
## gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
           x <<- y
           s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## calculates the inverse of the matrix if it hasn't been cached
## returns the inverse of matrix x

cacheSolve <- function(x, ...) {
         s <- x$getinverse()
         if(!is.null(s)){
                message("getting cached data")
                return(s)
         }
         data <- x$get()
         s <- solve(data, ...)
         x$setinverse(s)
         s
        ## Return a matrix that is the inverse of 'x'
}

