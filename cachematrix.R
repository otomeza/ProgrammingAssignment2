#Programming Assignment - Using cache 
#makeCacheMatrix: Creates a special "matrix" object that can cache the inverse of the matrix.
#Algorithm
##Initialize an object for the inverse
##Define objects to store the matrix and its inverse
##Return a list - special matrix object

#cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), cachesolve retrieves it from the cache.
#Algorithm
##Look for the inverse of the matrix passed has its inverse cached through the makeCacheMatrix function.
##Return the same if present, else compute and return


#Making a  cache for a matrix inverse
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL #initiating an object to hold the matrix inverse
    #Making a funciton to hold the matrix to be inversed. Can be used in other environments   
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(compinv) inv <<- compinv #compinv is the inverse computed outside this function
    getinv <- function() inv #gets the stored inverse whether computed or not
    
    #setmat, getmat, setinvmat, getinvmat are for external use
    list(setmat = set, getmat = get,
         setinvmat = setinv,
         getinvmat = getinv)
    
}

#Computing the inverse or returning the inverse if cache is available
cacheSolve <- function(x, ...) {
    
    inv <- x$getinvmat() #inverse taken from the makeCacheMatrix
    
    #Checking whether the inverse is present in the cache and returning it if it is there   
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #Computing the inverse of the matrix       
    data <- x$getmat()
    inv <- solve(data, ...)
    x$setinvmat(inv)
    inv
    
}
