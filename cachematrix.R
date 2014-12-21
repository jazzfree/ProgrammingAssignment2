## makeCacheMatrix stores a matrix given to a variable in a value of x which is available in 
## multiple environments. After storing the matrix plus 4 functions in the variable containing the 
## environments we work with one can set a stored version of the inverse of the matrix by calling cacheSolve
## having its input set on ...$getM() to get the matrix as input for the action solving the matrix.

makeCacheMatrix <- function(x = matrix()){
  sI <- NULL #on the creation of the matrix s is set to NULL
  setI <- function(y){
    x <<- y #to set the value of x in the list
    sI <<- NULL #on the set action the Solved inverse is reset to NULL
  }
  
  getM <- function() x #get the value of the matrix out of the variables stored.
  
  
  getInverse <- function() sI #return the stored value of the solve action (null if not solved).
  setInverse <- function(solve) sI <<- solve  #unlike the example function i found it better to really ad the solved matrix instead of a given value..
  
  #fill the variable with a list (or actually the functions)
  list(setI = setI, 
       getM = getM,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  
}

## cacheSolve solves the `matrix´ returning its inverse. If the inverse is set previously in the parent 
## environment then that value is returned, otherwise the matrix is solved and stored via the setM function.

cacheSolve <- function(x, ...){
  #get the value in x's inverse "function" returning the stored value within the variable (inside the main environment)
  sI <- x$getInverse()
  
  #if there is no value then go on, (!) when there is return the value in the 
  #<<- (assigned) value of sI. The assigning gets done in the end of this function via
  #the set inverse function in the stored list (here named x but in the overall environment).
  if(!is.null(sI)){
    message("getting stored solution")
    return(sI)
  }
  
  data <- x$getM()
  sI <- solve(data, ...)
  x$setInverse(sI)
  sI
}







