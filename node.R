# node object which returns a class 
# left:l is a node which represents the left part
# right:r is a node which represents the right part
# splitclass:SC is the split column 
# splitvar;SV is the actual split variable


setClass(
  "node",
  slots = c(
    data = "ANY",
    left = "ANY",
    right   = "ANY",
    splitcol   = "ANY",
    splitvar = "ANY",
    nodetype = "ANY"
  ), 
  prototype = list(
    nodetype = "normal_node"
  )
  
)
setClass(
  "leafnode",
  slots = c(
    label = "ANY",
    nodetype = "ANY"
  ),
  prototype = list(
    nodetype = "leaf_node"
  )
)

addbranch <- function(node, left, right) {
  node@left  <- left
  node@right <- right
  return(node)
}

rec_tree_build = function(node,I){
  if(I> 1) {
    node@left = rec_tree_build(new('node', left = I),I/2)
    node@right = rec_tree_build(new('node', left = I),I/2)
    
  }
  return(node)
  
}


