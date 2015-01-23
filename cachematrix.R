## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function take a matrix x as input
# matrix x is stored internally 
# set function : replaces the currnet matrix with a new one : parameter y
# get function : returns the current matrix
# setInvert : takes the input matrix y and stores it in variable invertedData. Functions assumes the input is the inverted matrix.
# getInvert : returns the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  # function takes a matrix as input
  invertedData <- NULL                        # holds the inverted data
  
  set <- function(y) {                       # set the data :: for example if we wanted to change the matrix
    x                 <<- y                  # x holds the matrix data        
    invertedData      <<- NULL               # if we call set, then we no longer know the inverted data so set it to NULL    
  }
  get       <- function() x                          # get returns the raw or original data
  setInvert <- function(y) invertedData <<- y        # takes the inverted data as input (y) and stores it in invertedData
  getInvert <- function() invertedData               # gets/returns the inverted matrix
  
  list(set = set, 
       get = get,
       setInvert = setInvert,
       getInvert = getInvert
  )   
}


## Write a short comment describing this function
# function takes matrix x as input
# intially the code asks the makeCacheMatrix for the inverted matrix
# if makeCacheMatrix already has a copy of the inverted matrix it is returned and the function exits
# other wise, the inversion is computed and setInvert is called to store it
# function returns the inverted matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # assumes x is of type 'makeCacheMatrix'
  invertedData <- x$getInvert()                                # gets the inverted matrix from the input variable x
  if(!is.null(invertedData)) {                                 # Check to see if the invert has been computed already
    message("Inversion already computed, returning data inverted data from the cache")
    return(invertedData)
  }
  
  # if the inversion has not been computed, compute it and store it in the x input
  message("Computing inversion....")
  x$setInvert(solve(x$get()))                     # get the original matrix, inverts it and places it back in makeCacheMAtrix
  invertedData <- x$getInvert()                   # get the inverted matrix from the makeCahceMatrix x
  invertedData                                    # return the inverted matrix
}
