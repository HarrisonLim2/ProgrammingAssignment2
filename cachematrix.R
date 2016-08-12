## makeCacheMatrix: stores matrix data, after calculating the inverse of a matrix you can
##                  cache the value in makeCacheMatrix so you do not need to recompute the inverse

## cacheSolve:  Inverses a makeCacheMatrix vector with the matrix data already stored in makeCacheMatrix
##              If the matrix has already been inverted it will use the cached value and not recompute
##              If the matrix has not been inverted it will compute the inverse and cache the value

## Comments At Bottom:  The comments at the bottom show the functions in action you can see the first time
##                      they are used we must compute the inverse but the next time we can use the cached value                        

## See above for description

makeCacheMatrix <- function(x = matrix()) {
  InverseMatrix <- NULL #Clear any saved Inverse matrix when function lists are created
  SetMatrixData <-function(NewUnInverseMatrix){
    x <<- NewUnInverseMatrix
    InverseMatrix <<- NULL #Clear any saved Inverse matrix 
  }
  GetMatrixData <- function() {x}
  SetInverseMatrix <-function(NewInverseMatrix) {InverseMatrix <<- NewInverseMatrix}
  GetInverseMatrix <-function() {InverseMatrix}
  list(SetMatrixData=SetMatrixData,GetMatrixData=GetMatrixData,SetInverseMatrix=SetInverseMatrix,GetInverseMatrix=GetInverseMatrix)
}

## See above for description

cacheSolve <- function(x, ...) {
  InverseMatrix <-x$GetInverseMatrix()
  if(!is.null(InverseMatrix)){
    print("Getting Cached Data")
    return(InverseMatrix)
  }
  print("Calculating Data")
  data <-x$GetMatrixData()
  InverseMatrix<-solve(data)
  x$SetInverseMatrix(InverseMatrix)
  InverseMatrix
  ## Return a matrix that is the inverse of 'x'
}


#Function Example

#CacheMatrix<-makeCacheMatrix(matrix(1:4,2,2))
#cacheSolve(CacheMatrix)
#cacheSolve(CacheMatrix)






