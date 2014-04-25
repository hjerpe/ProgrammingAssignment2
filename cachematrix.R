#Two functions for creating special matrix, where its inverse
#can be cached instead of repeatedly being computed.

makeCacheMatrix <- function(mat_A = numeric()) {
    #Function returns a list of functions wrapped in a list.
    # Args:
    #   mat_A       -   Matrix
    # Returns:
    #   set         -   Change matrix instead of creating 
    #                   new makeMatrix.
    #   get         -   Returns the matrix mat_A.
    #   setInverse  -   Sets inverse of mat_A and caches
    #                   the result.
    #   getinverse  -   Returns the inverse of mat_A.
    
    inv_A <- NULL
    #Change matrix entries instead of making a new makeMatrix 
    #object.
    set <- function(mat_B) {
        mat_A <<- mat_B
        inv_A <<- NULL
    }
    get <- function() mat_A
    setInverse <- function(inverse) inv_A <<- inverse
    getInverse <- function() inv_A
    #Returs four functions wrapped in a list.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(spec_A, ...) {
    #Client function for special matrix object makCacheMatrix
    #the function returns the inverse of special matrix object
    #and caches the result (we assume that the matrix is 
    #invertible).
    # Args:
    #   spec_A  -    makeMatrix object.
    # Returns:
    #   inv     -   inverse of matrix contained in spec_A.
    
    inv<- spec_A$getInverse()
    #Inverse already been computed.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #else we
    data <- spec_A$get()    #Get matrix from spec matrix A.
    inv <- solve(data, ...) #Compute inverse of matrix.
    spec_A$setInverse(inv)  #Add inverse to cache.
    inv                     #Return result. 
}