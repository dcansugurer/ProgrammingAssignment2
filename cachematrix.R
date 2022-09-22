## Functions for the caching of inverse matrix

# Function for creating object for cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse matrix
        inv <- NULL
        
        # Set the matrix
        setMatrix <- function(matrix) {
                m <<- matrix
                inv <<- NULL
        }
        
        # Get the matrix
        getMatrix <- function() {
                # Return to matrix
                m
        }
        
        # Set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Get the inverse of the matrix
        getInverse <- function() {
                # Return the inverse
                inv
        }
        
        # Return list of the methods: 
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}



# Function for computing the inverse of matrix 
# When the matrix is already inversed without changing, cacheSolve function 
# would retrieve the inverse of matrix. If not, it will calculate
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # Return if the inverse is already calculated:
        if (!is.null(m)) {
                message("Getting cached data!")
                return(m)
        }
        
        # Get the matrix
        mat <- x$getMatrix()
        
        # Compute the inverse of the matrix
        m <- solve(mat) %*% mat
        
        # Set the inverse to the object
        x$setInverse(m)
        
        # Return the matrix
        return(m)
        
}
