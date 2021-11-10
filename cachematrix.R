## Put comments here that give an overall description of what your
## functions do
## Trying to generate a function that calculates the inverse of a matrix using 
## the function solve() and stores it in cacheSolve
## This was made based on my understanding of the explanation made by Len Greski 
## (available in the forum)
## 
## Function set() takes a new numeric matrix as input
## 
## Function get() will return (print) the matrix fed as input
## 
## Function setinverse() defines the mutator for s solve
## 
## Function getinverse() defines the accessor for s solve

## Write a short comment describing this function


makeCacheMatrix <- function (x= matrix()){

        s <- NULL                                                               # solve is first set to NULL 
        set <- function (y){                                                    # nested function    
                x <<- y
                s <<- NULL
        }
        get <- function()x                                                      # this function will return the matrix 
        setinverse <- function (solve) s <<- solve                              # use func solve () for inverse
        getinverse <- function() s 
        list(set = set, get = get,                                              # this generates a list for the 4 
             setinverse = setinverse)                                           # functions that will be returned

        s <- NULL                                                               # solve is first set to NULL 
        set <- function (y){                                                    # nested function    
                x <<- y
                s <<- NULL
        }
        get <- function()x                                                      # this function will return the matrix 
        setinverse <- function (solve) s <<- solve                              # use func solve () for inverse
        getinverse <- function() s
        list(set = set, get = get,                                              # this generates a list for the 4 
             setinverse = setinverse,                                           # functions that will be returned

             getinverse = getinverse)
} 



## Write a short comment describing this function
## cacheSolve() will check the cache if there is a pre-calculated inverse
## if so it will return a massage I set if not calculated it will use the 
## new matrix to make the inverse 

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'

        s <- x$getinverse ()                                                    # call the getinverse() function in input
        if (!is.null(s)){                                                       # If 's' was already calculated (not NULL) a message 
                message("Hum -_-' is this a deja vu!? Here is what you're looking for:")         # will appear
                return(s)                                                       # pre-calculated matrix inverse is returned
        }
        InvMat<- x$get()                                                        # If not s is not existent (not calculated already) 
        s <- solve(InvMat, ...)                                                 # than new inverse for the new matrix will be 
        x$setinverse(s)                                                         # calculated

        s <- x$getinverse ()                                                    # call the getinverse() function in input
        if (!is.null(s)){                                                       # If 's' was already calculated (not NULL) a message 
                message("Hum -_-' is this a deja vu!? I think I 
                have calculated this for you before!
                        Here is what you're looking for:")                      # will appear
                return(s)                                                       # pre-calculated matrix inverse is returned
        }
        InvMat<- x$get()                                                        # If not s is not existent (not calculated already) 
        s <- solve(InvMat, ...)                                                 # than new inverse for the new matrix will be 
        x$setinverse(s)                                                         # calculated

        s
        
}


# Lets do some tests
#I'll create three matrices mat1, mat2 <- mat1 and mat3 to test my function
mat1<- matrix(1:4, 2,2)
mat2 <- mat1
mat3<- matrix(5:8, 2,2)


matinv<- makeCacheMatrix(mat1)
matinv$get()
cacheSolve(matinv)
matinv$getinverse()
#Check if my function can detect that I have already generated the inverse of mat1
cacheSolve(matinv)
#The function can't detect that I have already generated the inverse of mat1 when I called it now mat2
#It seems that it retains the name of the matrix (argument x) but not it's content  
matinv<- makeCacheMatrix(mat2)
matinv$get()
matinv$getinverse()
#Check the cache
cacheSolve(matinv)

matinv$set(mat3)
matinv$get()
matinv$getinverse()
cacheSolve(matinv)
#Check the cache again
cacheSolve(matinv)
