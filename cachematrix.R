## makeCacheMatrix inoput: inversible matrix   
## makeCacheMatrix output: an object with 4 functions:
   ## getting and setting matrix inverse value
   ## getting and setting the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  MInverse <- NULL
  
  setinverse<- function(inv){MInverse <<- inv}
  getinverse<- function(){MInverse}
  
  getmatrix<- function() x 
  setmatrix <- function(m) {
    x<<-m
    MInverse<<- Null
  }
  
  list(getinverse = getinverse, setinverse = setinverse, getmatrix = getmatrix, setmarix = setmatrix)
}


## cacheSolve input: an object of "makeCacheMatrix" with 2 objects: 1) Matrix 2) Matrix Inverse
## cacheSolve output: stores inversed matrix into "makeCacheMatrix" object

cacheSolve <- function(x, ...) {
 
  Inverse <- x$getinverse()

  if (is.null(Inverse)){      ## inversing the matrix and caching the data
    Mymatrix <- x$getmatrix() 
    I <- solve(Mymatrix)
    x$setinverse(I)
    I
  }
  else                        ## getting cached data
    return(Inverse)
    

}


