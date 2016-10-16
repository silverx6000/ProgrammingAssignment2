
## This first function "makeCacheMatrix" generates a list of "getter" & "setter" methods. The methods assign (set) and retreive (get) a given matrix and its respective inverse in an intrinsic environment variable,
## The second function "cacheSolve" is passed the list from the first function "makeCacheMatrix" and attempts to calculate and set its inverse.  If the inverse is already set, teh cached value is used

## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x
                            = matrix()) {
        ## First initialise the inverse matrix
        cachedInvMatrix <- NULL 
        
        ## Set matrix method: 
        ## Firstly, set x in parent environment with the required object (chosen matrix).  
        ## Secondly, clear the inverse matrix object if it has a value.
        
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedInvMatrix <<- NULL
        }
        
        ## Get Matrix method:
        ## Method returns the input matrix the inversion is being performed on.
        get <- function() x
        
        ## Inverse Setter method:
        ##set inverse variable in parent environment to desired value and return the value for verification 
        # or testing
        setInverse <- function(inverseValue) {
                
                cachedInvMatrix <<- inverseValue 
                
                #Comment out next line if you don't want the set value to be printed.
                return(cachedInvMatrix) 
        }
        
        ## Get Inverse Matrix method:
        ## Method returns the inverse of the matrix..
        getInverse  <- function() cachedInvMatrix
        
        #Finally all methods are added to a list.
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

## Given the list variable from the first function makeCacheMatrix, first a check is for a cached inverse to then return
## If not the inverse, where possible is calculated, set and returned etc.

cacheSolve <- function(x=makeCacheMatrix(1:9, nrow=3, ncol=3), ...) { 
        ##Default 3x3 matrix with elements 1:9 created if matrix is a particular matrix
        #is not the input

        
        ## First set the cachedInverse variable to test for a cached result
        cachedInverse <- x$getInverse() 
        
        ##if statement to chech if there's a cached value AND ensure it's a matrix
        if(!is.null(cachedInverse) && is.matrix(cachedInverse)) { 
                message("Calculation already done, cached data returned")
                return(cachedInverse)
        }
        
        
        ## If no cached value is found get the matrix and attempt to solve
        matrixToSolve <- x$get()  
        
        ## try to solve the matrix and catch errors and warnings
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, warning=function(w) {
                message("This may not be the result you're looking for")
                message(w)
        }, error=function(e) {
                message("Something went wrong solving your matrix")
                message(e)
                message("\n")
        })
        
        ## whatever the case, set the value of the inverse (NULL if something went wrong)
        message("Setting the value of inverse to:") 
        x$setInverse(calculatedInverse)
        
        
}