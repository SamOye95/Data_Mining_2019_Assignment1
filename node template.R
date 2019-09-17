# node object which returns a class 
# left:l is a node which represents the left part
# right:r is a node which represents the right part
# splitclass:SC is the split column 
# splitvar;SV is the actual split variable


node<- setClass(
  "node",
  slots = c(
    left = "ANY",
    right   = "ANY",
    splitclass   = "ANY",
    splitvar = "ANY"
  ),
  
)
Leafnode<- setClass(
  "leafnode",
  slots = c(
    label = "any"
  )
)
