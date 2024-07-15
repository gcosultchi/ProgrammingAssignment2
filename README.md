### Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  
        set <- function(y) {
                x <<- y
                inv <<- NULL  
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        mat <- x$get()
        
        inv <- solve(mat, ...)
        
        x$setInverse(inv)
        
        return(inv)
}

# Create a matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Create a special "matrix" object
cached_matrix <- makeCacheMatrix(my_matrix)

# Calculate the inverse and cache the result
inverse_matrix <- cacheSolve(cached_matrix)

# Retrieve the cached inverse
cached_inverse_matrix <- cacheSolve(cached_matrix)

# The results
inverse_matrix
cached_inverse_matrix
