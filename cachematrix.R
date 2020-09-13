## Put comments here that give an overall description of what your
## functions do

#part 1
# This function create special "matrix" object.
makeCacheMatrix <- function(Matrix) {
  Inv <- NULL
  Set <- function(y) {
    Matrix <<- y
    Inv <<- NULL
  }
  Get <- function() Matrix
  SetInverse <- function(Inverse) Inv <<- Inverse
  GetInverse <- function() Inv
  list(Set = Set,
       Get = Get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
  
  
}

#part 2
# This function computes the inverse of the special "matrix" that created by  makeCacheMatrix and return,
#If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(Matrix, ...) {
  Inv <- Matrix$GetInverse()
  if (!is.null(Inv)) {
    message("Cached Data !!!")
    return(Inv)
  }
  Mat <- Matrix$Get()
  Inv <- solve(Mat, ...)
  Matrix$SetInverse(Inv)
  Inv
}

# Example work
a<-makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2))
cacheSolve(a)









