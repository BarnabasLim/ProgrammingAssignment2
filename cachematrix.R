## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. Define set() function allows user to reset matrix
## 2. Define get() function allows user to get matrix
## 3. Define setInverse() function usually not used by users but by cacheSolve function
## 4. Define getInverse() function usually not used by users but by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  
  ## 1. Define set() function allows user to reset matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## 2. Define get() function allows user to get matrix
  get <- function() x
  
  ## 3. Define setInverse() function usually not used by users but by cacheSolve function
  setInverse <- function(Inverse) i <<- Inverse
  
  ## 4. Define getInverse() function usually not used by users but by cacheSolve function
  getInverse <- function() i
  
  ## 5. A list of functions is returned
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## 1. Return a matrix that is the inverse of 'x'
## 2. if i is null, carry out the inverse
cacheSolve <- function(x, ...) {
  ## 1. Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## 2. if i is null
  data <- x$get()
  i <- solve(data)%*% data
  x$setInverse(i)
  i
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}