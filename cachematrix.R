## The following functions are used to create a 'special matrix' 
## cache it's inverse and retrieve the inverse from the cache

## This first function creates a 'special matrix' and caches its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  get <- function()x
  setmi <- function(inverse) mi <<- inverse
  getmi <- function() mi
  list(set = set, get = get, setmi = setmi, getmi = getmi)
}

## This function is meant to retrieve the inverse from the previous function.
## It first checks to see if x is a square invertible matrix.
## If so, it gets the invertible matrix from the cache and skips computation.
## Otherwise it calculates the inverse of the matrix via the solve function
cacheSolve <- function(x, ...) {
        mi <- x$getmi()
        if(!is.null(mi)){
          message("getting cached data")
          return(mi)
        }
        mat <- x$get()
        mi <- solve(mat)
        x$setmi(mi)
        mi
}


# ProgAssig2
