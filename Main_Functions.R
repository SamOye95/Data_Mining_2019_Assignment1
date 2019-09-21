#Data Mining 2019 Assignment 1
#Created by Samuel Oyediran (6533809) and Nick Smidt
# This script contains the implementation of classification trees

source("Util_Functions.r")

# Function: tree.grow(x, y, nmin, minleaf, impurity)
# Trains a binary classification tree.
#
# Arguments
#   x : A numerical matrix. Each column contains observations for a
#       a numerical/binary attribute.
#   y : A numerical (binary) vector, containing the true class label
#       for each row of x
#   nmin : an integer representing the minimum number of observation that a node
#          must contain for it to be allowed to be split
#   minleaf : an integer representing the minimum number for a leaf node
#   impurity : The impurity function that will be used in the algorithm to
#              compute the impurity reduction (default = gini index)
#
#
# The number of rows of x is the same as the length of y.
#
# Result: A tree data structure, that can be used to predict class labels
#         with the classify function.

tree.grow <- function(x, y, nmin = 0, minleaf = 0, impurity = gini_index){
  #TODO 
  
}


# Function: tree.classify(x, tr)
# Predicts the class label for each row in the input attributes matrix.
#
# Arguments
#   x : A matrix with the same number of columns as the matrix used to train tr
#   tr : A tree object produced by the tree.grow function
#
# Result
#   A vector of binary class labels. It contains the predicted class label
#   for each row in x.
tree.classify <- function (x, tr){
  
  apply(x,1,predict, tr)
  
}

# Function: predict(x, tr)
# Predicts the class label for a single attributes input vector
#
# Arguments:
#   x : A numerical vector containing the attributes observations
#   tr : A tree object produced by the tree.grow function
#
# Result
#  The binary class label predicted by the trained tree.
predict <- function(x, tr){
  i <- 1
  while (is.na(tr$label[i]))
    i <- if (x[tr$splitCol[i]] <= tr$split[i]) tr$left[i] else tr$right[i]
  return(tr$label[i])
}

# Function: mkLeaf(y)
# Proudces a leaf row
#
# Arguments
#   y : A vector of binary class labels
#
# Result
#   A row representing a leaf node in the tree data structure.
#   All the fields are set to NA, except for label, which is computed applying
#   majority vote to the class label vector y.
mkLeaf <- function(y){
  
 c(NA, NA, majority_class(y), NA, NA)
  
}



# Function: mkNode(left, right, best)
# Produces an internal node row.
#
# Arguments
#   left, right : Integer numbers representing the row index at which the left
#                 and right are stored in the tree data structure.
#   best : A split object containing the fields split (threshold value) and
#          index, which is the column number of the attribute matrix in which
#          the same attribute values are stored
#
# Result
#  A vector representing an internal node row in the tree data structure.
#  The label field is set to NA.
mkNode <- function (left.index, right.index, best) {
  c(left.index, right.index, NA, best$split, best$index)
}