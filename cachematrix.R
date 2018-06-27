#########################################################################
### Peer-graded Assignment: Programming Assignment 2: Lexical Scoping ###
#########################################################################

#####################################
### Overall program description : ###
#####################################

# This assignment consists of two functions.
# The first function makeCacheMatrix takes a given matrix as an argument and provides routines 
# to store the inverse of the matrix.
# The second function cacheSolve computes and stores the inverse of the matrix only if it
# hasn't been cached yet.

###############################################################################################

##############################
### Function description : ###
##############################

# First of all the argument of this function is a matrix. 
# The body of this function will use this input matrix like this:
# Set the variable inv_matrix as empty matrix (placeholder for future matrix values).
# The function transf with argument a matrix is using in both operations the new introduced
# operator which can be used to assign a value to an object in an environment that is different 
# from the current environment.
# Basically we set matrix x with a new matrix y and 
# convert inv_matrix again into a placeholder.
# The redeem function returns matrix x.
# The setinverse function takes the inverse as an argument and stores it.
# The getinverse function returns the inverse matrix
# The list at the buttom provides the functions that allow the second function to set and get 
# the matrix inverse.

##############################################################################################

makeCacheMatrix <- function(x = matrix()) {
                   
                   inv_matrix <- NULL #matrix()
                   
                   transf <- function(y){
                             x <<- y
                             inv_matrix <<- NULL #matrix()
                   } 
                   
                   redeem <- function () x 
                   setinverse <- function(inverse) inv_matrix <<- inverse 
                   getinverse <- function() inv_matrix
                   
                   list(transf=transf, redeem=redeem, setinverse=setinverse, 
                        getinverse=getinverse)
}
#############################################################################################

##############################
### Function description : ###
##############################

# Using the information from the function above we are verifying 
# if we cached the inverse matrix
# We are saving the cached inverse matrix in variable and with help of the
# !is.null() statement we are checking if the stored matrix is not empty.
# Then a message is printed that confirms that the cached version has been used.
# If the if-statement is TRUE the other 3 statements will be disregarded.
# If not then we have to calculate the inverse matrix by hand.

#############################################################################

cacheSolve <- function(x, ...) {
              inv_matrix <- x$getinverse()
              if (!is.null(inv_matrix)) {
                 message("The inverse of the 'special matrix' is cached !")
                 return(inv_matrix)
              }
              specmatrix <- x$redeem()
              inv_matrix <- solve(specmatrix, ...)
              x$setinverse(inv_matrix)
              inv_matrix
  
}
#############################################################################