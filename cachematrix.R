## Put comments here that give an overall description of what your
## functions do

#Creates an container object that stores matrix data and its inverse
#also allows the retrieval of its matrix data and inverse value
makeCacheMatrix <- function(x = matrix()) {
    #initialize the matrix inverse value to NULL
    inv <- NULL
    #sets the makeCacheMatrix object from a user defined matrix argument
    #also initializes the inverse value to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    #returns the matrix
    get <- function() x
    #func to set inv value to calculated inverse argument
    setInverse <- function(inv.arg) inv <<- inv.arg
    #returns the inverse value
    getInverse <- function() inv
    #returns a list of the available object functions
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#takes a makeCacheMatrix object as an argument and returns its inverse
#the inverse will either be retrieved from a cache if possible
#otherwise the inverse will be calculated and then updated in the makeCacheMatrix 
#object
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #get the inverse from a makeCacheMatrix object
    inv <- x$getInverse()
    #check to see if an inverse has been calculated
    #if calculated, return value from cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #otherwise, retrive the matrix, 
    data <- x$get()
    #calculate the matrix
    inv <- solve(data, ...)
    #set the inverse into the cache of the makeCacheMatrix object
    x$setInverse(inv)
    #and finally return the inverse value
    inv
}
