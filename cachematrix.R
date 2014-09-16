#================================================================
# File:         cachematrix.R   
# Name:         Programming Assignment 2: Caching the Inverse of 
#               a Matrix
# Desc:         This file contains functions that both take the 
#               inverse of a matrix and cache its value for 
#               faster access
# Auth:         Ryan Samo
# Date:         9-15-2014
# Test:         Call like...
#               m <- matrix(data = 1:4, nrow = 2, ncol = 2)
#               z <- makeCacheMatrix(m)
#               cacheSolve(z)
#================================================================

#================================================================
# Func:         makeCacheMatrix   
# Desc:         Returns a matrix if already cached otherwise it
#               caches and returns the matrix
# Auth:         Ryan Samo
# Date:         9-15-2014
#================================================================
makeCacheMatrix <- function(inputMatrix = matrix()) {
        
        inputCache <- NULL
        set <- function(setMatrix) {
                inputMatrix <<- setMatrix
                inputCache <<- NULL
        }
        get <- function() inputMatrix
        setCache <- function(solve) inputCache <<- solve
        getCache <- function() inputCache
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

#================================================================
# Func:         cacheSolve   
# Desc:         Returns the inverse of a given matrix
# Auth:         Ryan Samo
# Date:         9-15-2014
#================================================================
cacheSolve <- function(inputMatrix, ...) {
        
        inputCache <- inputMatrix$getCache()
        
        if(!is.null(inputCache)) {
                print("Getting cached data")
                return(inputCache)
        }
        print("Getting non-cached data")
        data <- inputMatrix$get()
        inputCache <- solve(data)
        inputMatrix$setCache(inputCache)
        inputCache
}