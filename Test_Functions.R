#The functions used to be used in the test.R script 

#True Positive (TP)
tp <- function(y_p, y_r) { 
  TP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 1) ) {
      TP <- TP + 1
    }
  }
  return(TP)
}

#True Negative (TN)
tn <- function(y_p, y_r) { 
  TN <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 0) && (y_r[i] == 0) ) {
      TN <- TN + 1
    }
  }
  return(TN)
}


#False Negative (FN)
fn <- function(y_p, y_r) { 
  FN <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 0) && (y_r[i] == 1) ) {
      FN <- FN + 1
    }
  }
  return(FN)
}

#False Positive
fp <- function(y_p, y_r) { 
  FP <- 0
  for ( i in 1:length(y_p) ) {
    if ( (y_p[i] == 1) && (y_r[i] == 0) ) {
      FP <- FP + 1
    }
  }
  return(FP)
}

#Precision
precision <- function(y_p, y_r) { # TP/(TP + FP)
  TP <- tp(y_p, y_r)
  FP <- fp(y_p, y_r)
  return(TP/(TP + FP))
}


#Recall 
recall <- function(y_p, y_r) { # TP/(TP + FN)
  TP <- tp(y_p, y_r)
  FN <- fn(y_p, y_r)
  return(TP/(TP + FN))
}

#Accurancy
accurancy <- function(y_p, y_r) { # (TP + TN)/(TP + TN + FP + FN)
  TP <- tp(y_p, y_r)
  FN <- fn(y_p, y_r)
  TN <- tn(y_p, y_r)
  FP <- fp(y_p, y_r)
  return((TP + TN)/(TP + TN + FP + FN))
}





















