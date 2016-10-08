## Programing Assignment 2: Lexical Scoping

## Matrix inversion is usually a costly compution and there
## may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. This function will
## cache the inverse of a matrix. :-)

## This function creates a special "mastix" object that can cache
## its inverse.

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


## This function computes the inverse of a special "matrix"
## returned by makeCacheMatrix above. If hte inverse has been
## calculated already, then the cacheSolve function should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("Getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

# test:
# > x <- rbind(c(5, 2.5), c(2.5, 5))
# > x
# [,1] [,2]
# [1,]  5.0  2.5
# [2,]  2.5  5.0
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]  5.0  2.5
# [2,]  2.5  5.0
# > cacheSolve(m)        -- first attempt no cache
# [,1]       [,2]
# [1,]  0.2666667 -0.1333333
# [2,] -0.1333333  0.2666667
# > cacheSolve(m)
# Getting cached data.   -- second attempt retrieves from cache
# [,1]       [,2]
# [1,]  0.2666667 -0.1333333
# [2,] -0.1333333  0.2666667
# > 