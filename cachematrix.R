## For a large matrix, it may take too long to compute its inverse, 
##especially if it has to be computed repeatedly. 
##If the contents of the matrix are not changing, it may make sense 
##to cache the value of its inverse so that when we need it again, 
##it can be looked up in the cache rather than recomputed

## 1.-This function creates a special "matrix" object that can cache 
##its inverse.

makeCacheMatrix <- function(M = matrix()) {
    
    inv_M <- NULL
    
    set <- function(A) {    ## Set the value of the matrix
        M <<- A
        inv_M <<- NULL
    }
    
    get <- function() M      ## Get the value of the matrix
    
    setinverse <- function(Minverse) inv_M <<- Minverse     ## Set the value of the inverse matrix
    
    getinverse <- function() inv_M       ## Get the value of the inverse matrix
    
    list (set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## 2.-The next function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(M = matrix(), ...) {
    
    inv_M <- M$getinverse()  
    
    if(!is.null(inv_M) && solve(inv_M) == M) {    ## Check if the inverse has been calculated   
        message("getting cached data")            ## and if the matrix has not changed 
        return(inv_M) }
    
    
    data <- M$get()             ## If the inverse doesn't exist or the matrix is not the same 
    ## then we compute the inverse
    inv_M <- solve(data, ...) 
    
    M$setinverse(inv_M)
    
    inv_M
}

## Execute in R for a given square matrix: cacheSolve(makeCacheMatrix())