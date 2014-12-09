## This implementation fo the cached matrix inversion function


## This is factory function used to initialise the matrix with the
## cache options. Assigne it to the object you want to used as cached matrix

makeCacheMatrix <- function(x = matrix()) {
    ## check matrix at construction time. We do not want to use non square matrix
    if(nrow(x) != ncol(x)) {
        stop("Only square matrices can be cached")
    }

    mi <- NULL #  var will store cached inversed matrix
    set <- function(y) {
        if(nrow(y) != ncol(y)) {
            stop("Only square matrices can be cached")
        }
        x <<- y # stores the original matrix
        mi <<- NULL # original changed, we have to reset cache
    }
    get <- function() x ## return matrix itself
    setmi <- function(inversed) mi <<- inversed # store calculated value
    getmi <- function() mi # return cahed value
    list(set = set, get = get,
         setmi = setmi,
         getmi = getmi) # makes functions visible from the outside
}


## caclulate inverse matrix for the "cachable" matrix. Will use cached value or
## calculate and store value in cache if cache was not available

cacheSolve <- function(x, ...) {
    ## check if we are using the object of the right class
    if ( (class(x) != "list") || ( class(x$getmi) != "function" ) ) {
        stop("Wrong object type. You have to create matrix using makeCacheMatrix function")
    }
    mi <- x$getmi()
    if (!is.null(mi)) {
        message("getting cached data") # inform user that cached data used
        return(mi) #return cached result
    }

    ## no cache. need to calculate
    data <- x$get() # get original matrix
    mi <- solve(data, ...) #calculate value passing extra parameters if present
    x$setmi(mi) # save result

    mi # Return a matrix that is the inverse of 'x'
}
