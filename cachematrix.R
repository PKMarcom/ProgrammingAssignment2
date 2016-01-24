## Put comments here that give an overall description of what your
## functions do

#Programming Assignment 2 to understand lexigraphical scoping and get more practice with 
#defining functions.  The code below is based on the sample code given for the makeVector
#and cachemean functions.  

#the curly braces in makeCacheMatrix help me, as a beginner in R, understand the scope of
#individual functions that are then passed to the List function. The post "assign 2 how
#i run the code to see if I understand it?" and blog post "https://asitarrives.wordpress.
#com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-
#in-coursera/" were very helpful for figuring this out and understanding what the 
#code was doing.

## Write a short comment describing this function:  makeCacheMatrix is a function that accepts
#the matrix to be inversed, and then sets up four functions for storing and retrieving the
#original matrix (set and get) and the inverse matrix (setInverse and getInverse).  When
#called, by typing makeCacheMatrix(m), where "m" is a defined matrix object,
#it makes the inv null and puts the matrix in a variable x using the double assignment
#so that it is available outside the set function.  The setInverse and getInverse are 
#used by the cacheSolve function to "cache" the inverse of the matrix and be able to
#recall it.

makeCacheMatrix <- function(x = matrix()){
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y       #makes the matrix passed to the function available as "x"
                #for use in cacheSolve.
                
                inv <<- NULL  #makes the inv null outside the scope of the function
        }
        get <- function() {
                x
        }
        
        setInverse <- function(inverse) {
                inv <<- inverse #makes the inv variable available to cacheSolve
        }
        
        getInverse <- function() {
                inv
        }
        
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
} 


## Write a short comment describing this function: cachesolve accepts the matrix, checks to
#see if it has been "solved" (i.e. inverse calculated) by looking to see if getInverse
#has a value.  If so, it retrieves the cached result.  If "null", it retrieves the 
#matrix with get(), solves it, (inv<-solve(data, ....)), and passes the result to the
#setInverse(inv) function so that next time it can just be retrieved, and then returns
#the result (but only when it is the first time solving the matrix)

cacheSolve <- function(x, ...){
        inv <- x$getInverse ()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv   ## Return a matrix that is the inverse of 'x'
} 
