# this function  creates a list
# that contains 4 member functions: set, get, setInve
# and getInve.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL # creat a empty matrix called "inver" where the data will be stored.

set <- function(y) {  # set the value of the vector.
        x <<- y
        inver <<- NULL # it initialises inver to null.
}
get <- function() x # return the input matrix
setInve <- function(inv) inver <<- inv # set the inversed matrix
getInve <- function() inver #returned the matrix already inverted.
list(set = set, get = get,
     setInve=setInve,
     getInve=getInve)
## return a list which have the function "inver".
}

#The following function calculates the inverse  of a special "vector". First it's checked
# to see If the inverse was calculated. If so, It is get the inverse and go ahead.'
#Otherwise, Is calculates the inverse of the data and sets the value of the inverse in the cache via de "setInve" 
#function.

cacheSolve <- function(x, ...) {
                m <- x$getInve()# get the inversed matrix from x.
        if(!is.null(m)) {  # if the inversion result is there
                message("getting cached data")
                return(m) ## return the calculated inversion.
        }
        data <- x$get()# # if not, we do x$get to get the matrix object.
        m <- solve(data)
        x$setInv(m)#set it to the object.
        m  ## return the solved result.
}
 