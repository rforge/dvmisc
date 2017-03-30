list.override <- function(list1, list2) {
  
  # Get names of elements of list1 and list2
  names.list1 <- names(list1)
  names.list2 <- names(list2)
  
  # Loop through elements of list 2. If in list 1, remove, then add; if not in list 1, add.
  for (ii in 1: length(list2)) {
    
    element.name <- names.list2[ii]
    loc.list1 <- which(names.list1 == element.name)
    if (length(loc.list1) > 0) {
      list1[loc.list1] <- list2[ii]
    } else {
      list1 <- c(list1, list2[ii])
    }
    
  }
  
  # Return list1, which has its original elements plus any extras/overrides from list2
  return(list1)
  
}