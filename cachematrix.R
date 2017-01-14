##  1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  2.	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then 
##  the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix() - This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    ## initialize InverseMatric to NULL
    InverseMatrix <- NULL
    
    ## create a "setter" for the matrix and initialize InverseMatrix to NULL
        set <- function (y){
          x <<- y  
          InverseMatrix <<- NULL  
    }
    
    ## create a "getter" for the matrix
    ## get returns x, stored in the main function. Does not require any input.
    get <- function() x
    
    ## create a "setter" for the inverse
    setInverse <- function(Inverse) InverseMatrix <<- Inverse
    
    ## create a "getter" for the inverse
    getInverse <- function() InverseMatrix
    
    ## finally, put the 4 functions in a list & return the list to the parent environment
    
    list(set = set,           # gives the name 'set' to the set() function defined above
         get = get,           # gives the name 'get' to the get() function defined above   
         setInverse = setInverse, #idem
         getInverse = getInverse) #idem
} 

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x' 
        ## For this assignment, assume that the matrix supplied is always invertible
        ## in other words, we do not need to check whether the matrix is empty or contains NAs
        ## (because in those cases, the matrix would not be invertible)
    InverseMatrix <- x$getInverse()
    if(!is.null(InverseMatrix)){          
        message("getting cached data")
        return(InverseMatrix)
    }
    ## if statement TRUE => function ends. 
    ## if statement FALSE => InverseMatrix is null (i.o.w. cache is empty) => code below will be executed
    
    data <- x$get()                 #retrieves data from makeCacheMatrix (=the matrix to be inverted)
    InverseMatrix <-solve(data,...) #by defintion, solve calculates the inverse of a matrix. 
    x$setInverse(InverseMatrix)     #set the inverse in the object generated / assigned with makeCacheMatrix
    InverseMatrix                   #return the newly calculated InverseMatrix
   } 
