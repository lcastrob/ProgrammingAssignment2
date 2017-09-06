## Luis Castro
##R Assigment
##  create matrix object and cache the inverse for futire calculations.

makeCacheMatrix <- function(x = matrix()) {
        ## x is  a  matrix
      
        
        invMat = NULL
        
        set = function(value) {
      ## to set the matrix value
                invmat <<- NULL
                
                x <<-value
                
                
        }
        
        ## return the value of X
        get = function() x
         
        ##return the value in inverse
        getinv = function() invmat
        
        ## reset the value ft the inverse
        setinv = function(inverse) invmat <<- inverse 
       
        
        ## list with the functions
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}




## this function will check if it has the value already calculeted
## if not it will calculate it


cacheSolve <- function(x, ...) {
        
        
        invmat = x$getinv()
        
        # check if we have already calculated the inverse
        if (!is.null(invmat)){
                
                ##if we have it return the cached one and avoid calculation
                message("getting cached data")
                return(invmat)
        }
        
        # if not already calculated then make the calculation
        mat.data = x$get()
        invmat = solve(mat.data, ...)
        
        # 
        ##use the set function to store the inverse for future calculations
        x$setinv(invmat)
        
        return(invmat)
}
