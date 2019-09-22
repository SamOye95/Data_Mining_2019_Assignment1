
# This script contains tests used to check our implementation

source("Util_Functions.r")
source("Main_Functions.r")

# Grows a classification tree on the credit data set used in the lectures.
# It returns the same classification tree. (The tree is actually symmetric to
# that one, because observations that satisfy a <= split are always assigned
# to the left child).
test.credit <- function(){
  credit <- read.data("credit.txt", 0, header = TRUE)
  return(tree.grow(credit$train.x, credit$train.y, nmin = 2, minleaf = 1))
}