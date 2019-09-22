# This script contains utility functions common to all the other scripts.

# Function: impurity_gini_index(y)
# Gini index impurity function for the two-class case
#
# Arguments:
#   y : A binary (numerical) vector, with class labels 0 or 1.
#
# Result: The value of the gini index impurity function for the given class label vector

impurity_gini_index <-function(x){
  #length of vector
   n <- NROW(x)
   #
   n1 <-sum(x)
   #returns gini index value
   return((n1 / n) * (1- (n1 / n)))
}
#nvals = numbervalues 
#cvals = classvalues
BNS<- function(nvals,cvals){
  classvals<- data.frame(cvals)
  n<-length(nvals)
  imp <- impurity_gini_index(classvals)
  splitval<- vector(mode = "numeric", n)
   i=1
  for (var in nvals) {
    left_total <- length(nvals[nvals<=var])
    left_possitive <- sum(cvals[nvals<=var])
    left_negative <- left_total-left_possitive
    
    right_total <- length(nvals[nvals>var])
    right_possitive <- sum(cvals[nvals>var])
    right_negative <- right_total-right_possitive
   # return(c(left_negative,left_possitive,left_total, right_negative, right_possitive, right_total))
    splitval[i]<- imp - (left_total /n)*(left_possitive/left_total)*(left_negative/left_total) - (right_total /n)*(right_possitive/right_total)*(right_negative/right_total)
    
    i<-i+1
  }
   val = nvals[which.max(splitval)]
  return(c(splitval[which.max(splitval)],val))

}

Bestsplit = function(data){
  list = setNames(data.frame(matrix(ncol = 3, nrow = 1)), c("infogain", "splitval", "column"))
  I =1
  for(var in data){
    uni= unique(var)
    if(length(uni) == 2){
      list = rbind(list,c(igi(var, data$class),1, colnames(data[I])))
      
     
    }
    else{
     list = rbind(list,c(BNS(var, data$class),colnames(data[I])))
      
    }
    
    I= I+1
  }
  return(list[which.max(list$infogain),])
}
# impurity gini index of a vector
igi = function(data, cvals){
  classvals<- data.frame(cvals)
  n = length(data)
  n1 = sum(data)
  return(impurity_gini_index(classvals)-(n1 / n) * (1- (n1 / n)))
}



# Function: split(s, x, y)
# Splits the vectors x and y according to the value of s.
#
# Arguments
#   s : A number representing the threshold value to be used to apply the split
#   x : A vector containing the values for some binary or numerical attribute
#   y : A vector containing the respecitve binary class labels for the observations contained in x
#
# x and y have the same length.
#
# Result:
# A list containing the following named fields : left, right, isRight.
# left and right are also lists containing two named fields (x and y).
# right$x and right$y are numerical vectors containing the portion of the input
# vectors x and y for which the x-values are greater than s.
# right$x and right$y contain the remaining values from x and y.
# isRight is a logical vector resulting from x > s
split <- function(s, x, y){
  isRight = x > s
  l = list(x = x[! isRight], y = y[! isRight])
  r = list(x = x[isRight]  , y = y[isRight])
  
  #returns list based on split s
  return(list(left = l, right = r, isRight = isRight))
}


# Function: reduction(s, x, y, i)
# Arguments
#   s : A number representing a treshold value for a split
#   x : A numerical vector containing values for a binary/numerical attribute
#   y : A numerical (binary) vector containing the class labels related to x
#   i : The impurity function to be used (default = gini_index)
#
# The vectors x and y have the same length.
#
# Result: A number representing the reduction obtained applying the split s.
reduction <- function (s, x, y, i = gini_index){
  nodes = split(s, x, y)
  l = nodes$left$y
  r = nodes$right$y
  pl = length(l) / length(x)
  pr = 1 - pl
  
  #returns the reduction value based on the split s
  return((i(l) * pl) + (i(r) * pr))
}


# Function: impurity_reduction(s, x, y, i)
#
# Arguments:
#   s : A number representing the split on the numerical attributes
#   x : A vector containing the numerical attributes
#   y : A vector containing the binary class labels
#   i : The impurity function used (default = gini_index)
#
# The vectors x and y have the same length.
#
# Result
# A number representing the impurity reduction obtained using the
# split s on x and y and the impurity function x.

impurity_reduction <- function (s, x, y, i = gini_index) {
  #returns the impurity reduction value
  return(i(y) - reduction(s, x, y, i))
}

# Function: partition(isRight, x, y)
# Partition the attributes matrix x and the class labels y according to the
# logical vector isRight.
#
# Arguments:
#   isRight : A logical vector
#   x       : A numerical matrix
#   y       : A numerical (binary) vector
#
# The number of rows of x and the length of isRight and y are the same.
#
# Result
# A list containing the following named fields:
#   left.x : left rows of x
#   right.x : right rows of x
#   left.y : left rows of y
#   right.y : right rows of y
# Where left or right is determined by the logical value of the correspondent
# element of isRight.
partition <- function(isRight, x, y) {
  rx = x[isRight, , drop = FALSE]
  lx = x[! isRight, , drop = FALSE]
  ry = y[isRight]
  ly = y[! isRight]
  return(list(left.x = lx, right.x = rx, left.y = ly, right.y = ry))
}