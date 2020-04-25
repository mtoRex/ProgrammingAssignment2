## Update two function makeCacheMatrix & cacheSolve with proper evalution
## First one is makeCacheMatrix where some variable and function intialized 
## Second one cacheSolve get putMark funcation called and returned inverse matrix.


makeCacheMatrix <- function(mat = matrix()) {
  m_val <- NULL
  set <- function(y_val) {
    mat <<- y_val
    m_val <<- NULL
  }
  get <- function() mat
  setResult <- function(solve) m_val <<- solve
  getResult <- function() m_val
        
  # list constructor
  list(set = set, get = get,
       setResult = setResult,
       getResult = getResult)
}

cacheSolve <- function(mat, ...) {
  m_val <- mat$getResult() ## put the function value in m_val 
  if(!is.null(m_val)) {
    message("getting cached data")
    return(m_val) ## Return the matrix that is the inverse of 'mat'
  }
  data <- mat$get()
  m_val <- solve(data, ...)
  mat$setSolve(m_val)
  m_val
}
