#Data Mining 2019 Assignment 1
#Created by Samuel Oyediran (6533809) and Nick Smidt
# This script contains the implementation of classification trees

source("Util_Functions.r")
source("node.r")

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


boom <- new("node", data= data, nodetype="normalnode")
tree.grow <- function(tree, x, y, nmin , minleaf , impurity = gini_index){
  
  
  if(impurity_gini_index(tree@data$class)> 0 & NROW(tree@data) > nmin){
  #cat(NROW(tree@data))
  
  best = Bestsplit(tree@data)
  tree@splitcol = best$column
  tree@splitvar = best$splitval
    if(NROW(data[data[best$column] <= best$splitval,])> minleaf){
      tree@left = tree.grow(new('node',  data = tree@data[tree@data[best$column] <= best$splitval,]),1,1,2,1)
    }
    if(NROW(data[data[best$column] > best$splitval,])> minleaf){
      tree@right = tree.grow(new('node', data = tree@data[tree@data[best$column] > best$splitval,]),1,1,2,1)
    }
  
 
  }
  else{
    tree = new('leafnode',label = round((sum(tree@data$class)/ NROW(tree@data$class))))
  }

  return(tree)
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
  searchdata = setNames(data.frame(matrix(ncol = length(x), nrow = 1)), colnames(tr@data))
  for (I in 1:length(x)) {
    searchdata[,I] = x[I]
  }
  return(searchdata)
  
}


