
# This script contains tests used to check our implementation

source("Util_Functions.r")
source("Main_Functions.r")
source("Test_Functions.r")

# Grows a classification tree on the credit data set used in the lectures.
# It returns the same classification tree. (The tree is actually symmetric to
# that one, because observations that satisfy a <= split are always assigned
# to the left child).
test.credit <- function(){
  credit <- read.data("credit.txt",0, header = TRUE)
  return(tree.grow(credit$train.x, credit$train.y, nmin = 2, minleaf = 1))
}

test.eclipse.2 <- function(){
  eclipse.2 <- read.data("eclipse-metrics-packages-2.0.txt",0, header = TRUE)
  eclipse.2.tree <- tree.grow(eclipse.2$train.x, eclipse.2$train.y, nmin = 15, minleaf = 5)
  eclipse.2.actual <- tree.classify(eclipse.2$test.x, eclipse.2.tree)
  eclipse.2.expected <- eclipse.2$test.y
  return(table(eclipse.2.expected, eclipse.2.actual))
}


# Grows a classification tree on the complete pima data set and uses it to
# predict the training sample itself and finally returns the resulting
# confusion matrix. The matrix is almost identical to the one reported in the
# assignment. Different runs returns slight different versions, as ties are
# broken at random.
test.pima <- function(){
  pima <- read.pima.data("pima.txt", 0)
  pima.tree <- tree.grow(pima$train.x, pima$train.y, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$train.x, pima.tree)
  pima.expected <- pima$train.y
  return(table(pima.expected, pima.actual))
}