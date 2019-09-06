#Data Mining 2019 Assignment 1
#Created by Samuel Oyediran (6533809)

# Description: Classification Tree
# Returns; A classification tree
#  1. x: The data containing the attributes values to split
#  2. y: labels that fit to x.
#  3. nmin - minimum amount of rows needed to split.
#  4. minleaf - minimum number of leafs a node should have
#  5. nfeat = Number of features to sample.

tree.grow <- function(x,y, nmin=2, minleaf=1, nfeat=ncol(x)) {
  
  #first some checks
  if (is.null(x)) {
    stop("Feature table cannot be empty or null")
  }
  
  if (is.null(x)) {
    stop("Class label cannot be empty or null")
  }
  
  if (minleaf < 1) {
    stop("Must have at least 2 observations on a leaf node")
  }
  
  if (nmin <= 0) {
    stop("Minimum number of observations for a node has to be positive")
  }
  
  if (nfeat > ncol(x)) {
    stop("Cannot take a sample larger than the population.")
  }
  
  
  #Creating a n empty tree object
  tree <- data.frame(matrix(ncol=5,nrow=0))
  
  
}