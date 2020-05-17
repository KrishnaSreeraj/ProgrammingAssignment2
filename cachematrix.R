
## Caching the Inverse of a Matrix

## This function makeCacheMatrix gets a matrix as input, set the value of the
## matrix, set the inverse matrix and get the inverse matrix. The matrix
## object can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

	invMat <- NULL
	setMat <- function(y) {
		x <<- y
		invMat <<- NULL
	}

	getMat <- function() x
	setInv <- function(inverse) invMat <<- inverse
	getInv <- function() invMat
	
	list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	invMat <- x$getInv()
	if(!is.null(invMat)) {
		message("Getting Cached Invertible Matrix")
		return(invMat)
	}

	Mat_data <- x$getMat()
	invMat <- solve(Mat_data,...)
	x$setInv(invMat)
	return(invMat)
}
