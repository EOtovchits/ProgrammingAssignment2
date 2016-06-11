## Put comments here that give an overall description of what your
## functions do

## Here are a couple of funcs. to make work w/ hard computations
## execute faster. In them we caclulate the inverse of matrix
## only once. Once computed we cache the inverse value
## and in the future we use the saved cached value only

## Write a short comment describing this function

## This function holds the matrix to cache along w/ its
## cached value
makeCacheMatrix <- function(x = matrix()) {
  
  cachedResult <- NULL
  
  ## A simple method to check that argument is correct 
  check <- function(matr){
    if (!is.matrix(matr)){
      message("Argument is not a matrix")
      return (FALSE);
    }
    
    nCol <- ncol(matr)
    nRow <- nrow(matr)
    
    if(nCol != nRow){
      message("Argument is not a square matrix")
      return (FALSE);
    }
    
    TRUE;
  }
  
  ## Set the raw value
  set <- function(y) {
    
    if(!check(y)){
      return (FALSE);
    }
    
    value <<- y
    cachedResult <<- NULL
  }
  
  # Get the raw value
  get <- function() value
  
  # Set calculated value
  setsolve <- function(value) cachedResult <<- value
  
  # Get calculated value
  getsolve <- function() cachedResult
  
  # Create our list of methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve,
       check = check)
}


## Write a short comment describing this function

## This function tryes to use the caching object if it is 
## ready. If not - the inversing operation executed and
## calculated result is saved in caching object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  res <- x$getsolve()
  
  # Return cached value if present
  if(!is.null(res)) {
    message("getting cached data")
    return(res)
  }
  
  # If not - calculate & save it
  data <- x$get()
  res <- solve(data)
  x$setsolve(res)
  res
}
