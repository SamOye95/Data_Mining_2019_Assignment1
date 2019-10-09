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


tree.grow <- function( x, y, nmin = 0 , minleaf= 0 , nfeat =0, impurity = impurity_gini_index){
  
  N <- length(y)
  tree <- data.frame(left = rep(NA,N),
                     right = rep(NA,N),
                     label = rep(NA,N),
                     split = rep(NA,N),
                     splitCol = rep(NA, N)
                     )
  
  tree[1,] <-mkLeaf(y)
  worklist <- list(1) #List of leaf indexs that still need to be split
  samples <- list() #Observations ro index contained by each leaf
  samples[[1]] <- 1:length(y)
  freeRow <- 2
  
  while (length(worklist) != 0) {
    current.index <- worklist[[1]]
    worklist <- worklist[-1]
    current.samples <- samples[[current.index]]
   
    y.current <- y[current.samples]
    x.current <- x[current.samples, , drop = FALSE]
    
    #nfeat colums for the x.current
    x.current = x.current[,sample(ncol(x),nfeat)]
    
    
    if(impurity(y.current) > 0 & length(current.samples) >= nmin) {
    
      best <- best.split.of.all(x.current,y.current, minleaf, impurity)
      splitcolumn = grep(paste("^", paste(colnames(x.current[best$index]),"$", sep = ""), sep = ""),colnames(x))[1]
      best$index= splitcolumn
      
      if(is.null(best)){
        next
      }
      
      #Make leaves
      left.index <- freeRow
      right.index <- freeRow + 1
      tree[left.index ,] <- mkLeaf(y.current[!best$isRight])
      tree[right.index,] <- mkLeaf(y.current[best$isRight])
      
      #Split samples
      samples[[left.index]] <- current.samples[!best$isRight]
      samples[[right.index]] <- current.samples[best$isRight]
      
      #Add children to current node
      tree[current.index, ] <- mkNode(left.index, right.index, best)
      
      #Control iteration
      worklist <- c(worklist, left.index, right.index)
      freeRow <-right.index + 1
    }
  }
  

    
    
  tree= head(tree,freeRow)
  return(tree)
}


# Function: The function grows m trees and store them in a list
#
# Arguments
# x (matrix)
# y (vector)
# nmin (numeric)
# minleaf (numeric)
# m (numeric)
#
#

tree.grow.bag <- function(x, y, nmin, minleaf, m) {
  # Vector for trees
  trees <- list()
  
  # Loop as many times as wanted
  for (i in 1:m) {
    # Take samples from the data to use in tree growing
    random_rows <- sample(nrow(x), nrow(x), replace = TRUE)
    tmp_x <- x[random_rows, , drop = FALSE]
    tmp_y <- y[random_rows]
    # Grow tree using selected samples
    trees[[i]] <- tree.grow(tmp_x, tmp_y, nmin, minleaf)
  }
  # Return all trees
 return(trees)
}




# tree.classify.bag
# INPUT: trees (list), x (matrix)
# OUPUT: vector of predictions
# The function classifies the input records by majority rule.
tree.classify.bag <- function(trees, x) {
  predictions <- c()
  num_trees <- length(trees)
  
  for (r in 1:nrow(x)) {
    row <- x[r, , drop = FALSE]
    tmp_predictions <- c()
    for (t in 1:num_trees) {
      current_tree <- trees[[t]]
      p <- tree.classify(row, current_tree)
      tmp_predictions <- c(tmp_predictions, p)
    }
    
    n <- length(which(tmp_predictions == 0))
    if (n >= (num_trees / 2)) {
      predictions <- c(predictions, 0)
    } else {
      predictions <- c(predictions, 1)
    }
  }
  
  return(predictions)
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