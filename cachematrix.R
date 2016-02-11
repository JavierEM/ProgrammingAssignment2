# Those two functions are meant to be used combined. The first one "makeCacheMatrix"
# creates a list containing a function to cache and get values for the input matrix as well
# as the matrix inverse. The second function "cacheSolve" calculates the inverse 
# of a matrix but will check first is the value has been cached in which case it 
# will just get this value stored on the the list created with the first function.
# if the inverse is not cached, it will calculate the it, return the value and cache 
# the result


# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix inverse   
# 4.  get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()){
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    set_inv_matrix <- function(solve) inv_x <<- solve 
    get_inv_matrix <- function() inv_x
    list(set = set, get = get,
         set_inv_matrix = set_inv_matrix,
         get_inv_matrix = get_inv_matrix)
}


# this function checks is the inverse of a matrix exist 
# if it does exist, it will get the cached value
# if it doesn't exit it will calculate the inverse value
# and return it. It will also cache the calculated inverse


cacheSolve <- function(x, ...) {
    inv_x <- x$get_inv_matrix()
    if(!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    }
    data <- x$get()
    inv_x <- solve (data, ...)
    x$set_inv_matrix (inv_x)
    inv_x
}

