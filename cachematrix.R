
## makeCacheMatrix function (or object) takes as an input a matrix that can be inverted
## the matrix invertablity is not tested. The function is intended to be used to store matrix
## and it's inverse to improve efficiency in working with large matrices with CacheSolve funtion.

makeCacheMatrix <- function(x = matrix()) {
   #Set initial inverted matrix value to NULL
    inverseMatrix <- NULL
  
    #Define function for updating the matrix values
    set <- function(y) {
        x <<- y
    
        #When the matrix is updated the inverted matrix needs to be re-calculated
        inverseMatrix <<- NULL
    }
    #Function for returning the matrix
    get <- function() x
  
    #Function for saving and updating the the inverted matrix 
    setInverseMatrix <- function(inverse) inverseMatrix <<- inverse
  
    #Funtion for retuning the inverted matrix
    getInverseMatrix <- function() inverseMatrix
  
    #List to be returned from the function
    list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


 
## cacheSolve function takes as an input a list created with makeCacheMatrix 
## and returns the inverted matrix. The inverted matrix is calculated
## or if this has already been done the information is extracted from the list. Using 
## CacheSolve funtion with makeCacheMatrix function improves efficiency in working with large matrices

cacheSolve <- function(x, ...) {
    #Get the value of inverted matrix from the object x
    m <- x$getInverseMatrix()
    #If the value excists return the calculated value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #In all other cases calculate the value
    #Get the original matrix
     data <- x$get()
  
    #Calculate the inverse matrix
    m <- solve(data, ...)
    
    #Update the original object
    x$setInverseMatrix(m)
    
    #Return the new matrix
     m
}