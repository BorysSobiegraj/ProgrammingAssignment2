## Put comments here that give an overall description of what your
## functions do

# The program creates two objects
#
# 1. makeCacheMatrix which is a List/function object containing functions to store infromation 
# and change definition of a matrix and its inverse. The mentioned matrix about which we store
# information which we store information passed as argument of makeCacheMatrix function. 
#
# 2. cacheSolve is a very plain function which is suppose to return the inverse 
# of matrix stored in makeCacheMatrix. As a smart cookie :) befere doing the heavy lifting
# of matrix inversion it checks if makeCacheMatrix has infomration about the matrix inverse.
# If it has it iproduce a short printout and returns the value. If not it evaluetes the
# inverse and passes the result to makeCacheMatrix object for it to remember it in the future :)
#
# Even though I know that what I have written above is true it is hard to right about it
# in a clear, plain and simple way - I hope my atempt wasn't completly failed ;)


## Write a short comment describing this function

# General description is provided above. For more detail I will provide comments 
# in the function code
                            # here we pass a matrix object
makeCacheMatrix <- function(x = matrix()) {

    # here we make sure that we clean the inverse_matrix variable. it is mainly important if you
    # choose to redefine and existing makeCacheMatrix object
    inverse_matrix <- NULL
    
    # after creating a makeCacheMatrix you can choose to change the stored matrix either 
    # by calling the full makeCacheMatrix function or by the set_matrix function shown below.
    # besides redefining the matrix it clears any stored value ofthe inverse_matrix object
    # making sure that the list doesn't have incoherent data (inverse stored not matching the matrix)
    set_matrix <- function(y) {
        
        x <<- y
        inverse_matrix <<- NULL
    }
    
    # it creates a function which returns the matrix we chose to store in makeCacheMatrix object.
    # I am not sure if defining this storage as a function isn't overding it but I suppose
    # it is mostly a coherence and conceptual excersise
    get_matrix <- function() x
    
    # this function sets the value of the matrix inverse. It is important that it doesn'e evaluate
    # nothing. It only passes on  value passed to it. IMHO it makes function suseptible to
    # unintended use - someone can pass here something different than the true value of the
    # matrix inverse but again it is an excersise not a marketable produkt.
    set_inv_value <- function(passing_inverse) inverse_matrix <<- passing_inverse
    
    # this object simply returns the value of the inverse_matrix
    get_inv_value <- function() inverse_matrix
    
    # here is an important thing - even though the makeCacheMatrix is a function it
    # produces a list (of functions). Here (in the last line of the function) we define 
    # the content of the mentioned list
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inv_value = set_inv_value,
         get_inv_value = get_inv_value)
    
}


## Most of the explaining was done in the introduction so as before I will continue in the code

            # We define a function which takes the makeCacheMatrix type object as an argument.
            # We can also pass additional arguments for the solve function if need be :)
cacheSolve <- function(x, ...) {

    # we take out the value of the inverse of matrix stored in the makeCacheMatrix object and
    # pass it to a local variable of this function and use the local variable to check if
    # the elemnt of makeCacheMatrix which should store the inverse isn't empty ...
    m <- x$get_inv_value()
    if(!is.null(m)) {
        
        # .. if it is not empty we say that that we are getting the cashed data and we return the
        # inverse
        message("getting cached data")
        return(m)
    }
    
    # If on the other hand the inverse element of the list was empty we assigne the matrix stored
    # in the makeCacheMatrix as value of "data" variable, than evaluete the inverse of the 
    # matrix which is now stored in "data" and put the value in local variable "m".
    data <- x$get_matrix()
    m <- solve(data, ...)
    
    # now we only put the evaluated inverse into the makeCacheMatrix list for safe keeping
    # and then return the value of the inverse
    x$set_inv_value(m)
    m
}

# If you would like to use one of my 2 examples of Matrixes (3x3 and 4x4)
# than you are very welcome to :) 
Macierz <- matrix(c(2,5,7,6,3,4,5,-2,-3),nrow = 3,byrow = TRUE)
Macierz2 <- matrix(c(1,2,3,4,2,3,1,2,1,1,1,-1,1,0,-2,-6),nrow = 4,byrow = TRUE)



