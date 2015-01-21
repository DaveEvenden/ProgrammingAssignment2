# create a function that returns an object with a cached passed matrix and its inverse

makeCacheMatrix <- function(NewX = matrix()) {
    # nullify the *local* placeholder variable for the potentially extant inverse
    I <- NULL
    # create a function to assign values in the *parent* environment
    setMatrix <- function(NewY) {
        NewX <<- NewY
        I <<- NULL
    }
    # create a function to assign the input matrix X
    getMatrix <- function() NewX
    # create a function to assign the passed inverted matrix to the placeholder variable I
    setInverse <- function(Inverse) I <<- Inverse
    # create a function to get the value of the placeholder variable I
    getInverse <- function() I
    # create an output list for return
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

# create a function that inverts the matrix returned by makeCacheMatrix above. 
# If inverse exists and matrix not changed, then retrieve the inverse

cacheSolve <- function(x, ...) {
    # fetch what cached things there are
    I <- x$getInverse()
    OldX <- x$getMatrix()
    # if I exists and NewX hasn't changed
    if(!is.null(I) & all(OldX==NewX)) {
        message("matrix okay - getting cached inverse")
        return(I)
    }
    # calculate new Inverse and return it
    message("recalculating inverse")
    I <- solve(NewX)
    x$setInverse(I)
    I
}