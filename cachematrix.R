## This function creates a special matrix that computes and caches its own inverse
## to save computational time for repeated calculations on the same object
## it is made up of 2 parts - the first creates the matrix object and the second
## computes the inverse and stores it if it hasn't already been done

## this is the function defining the data structure of a matrix and inverse dyad

makeCacheMatrix <- function(x = matrix()) {
        mtx <- NULL
        set <- function(y){
          x <- y
          mtx <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mtx <<- solve
        getinv <- function() mtx
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function checks to see if 'x' already has an inverse and returns it or creates, caches and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtx <- x$getinv()
        if(!is.null(mtx)){
          message("getting cached data")
          return(mtx)
        }
        data <- x$get()
        mtx <- solve(data)
        x$setinv(mtx)
        mtx
}
