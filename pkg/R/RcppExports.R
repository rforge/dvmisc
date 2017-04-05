# Adds elements of list2 to list1, overwriting any elements with the same name.
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


# Create fixed number of groups covering range of input vector x
interval.groups <- function(x, num = 5, ...) {
  
  # Figure out break points to split x into even intervals spanning its range
  x.range <- range(x, na.rm = T)
  cut.breaks <- seq(x.range[1], x.range[2], diff(x.range) / num)
  cut.breaks[c(1, length(cut.breaks))] <- c(-Inf, Inf)
  
  # Create groups
  groups <- cut(x = x, breaks = cut.breaks)
  #groups <- cut(x = x, breaks = cut.breaks, ...)
  
  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ", paste(table(groups), collapse = ", "), ". ", num.missing, " missing.", sep = ""))
  return(groups)
  
}


# Create graph of mean +/- error bar for continuous variable vs. factor
means.graph <- function(y, group, error.bars = "t.ci", alpha = 0.05, 
                        plot.list = NULL, 
                        lines.list = NULL, 
                        axis.list = NULL) {
  
  # Drop missing values
  locs.missing <- which(is.na(y) | is.na(group))
  if (length(locs.missing) > 0) {
    y <- y[-locs.missing]
    group <- group[-locs.missing]
  }
  
  # Get name of y and group variables for axis labels
  y.varname <- deparse(substitute(y))
  group.varname <- deparse(substitute(group))
  
  # Get levels of groups variable for tick labels
  group.levels <- levels(group)
  
  # Get group means
  means <- tapply(X = y, INDEX = group, FUN = mean)
  
  # Create error bars
  if (error.bars != "none") {
    
    if (error.bars == "sd") {
      
      sds <- tapply(X = y, INDEX = group, FUN = sd)
      lower.bars <- means - sds
      upper.bars <- means + sds
      y.label <- paste(y.varname, " (Mean +/- 1 SD)", sep = "")
      
    } else if (error.bars == "se") {
      
      ses <- tapply(X = y, INDEX = group, FUN = function(x) sd(x) / sqrt(length(x)))
      lower.bars <- means - ses
      upper.bars <- means + ses
      y.label <- paste(y.varname, " (Mean +/- 1 SE)", sep = "")
      
    } else if (error.bars == "t.ci") {
      
      ns <- tapply(X = y, INDEX = group, FUN = length)
      sds <- tapply(X = y, INDEX = group, FUN = sd)
      t.crit <- qt(p = 1 - alpha/2, df = ns - 1)
      lower.bars <- means - t.crit * sds / sqrt(ns)
      upper.bars <- means + t.crit * sds / sqrt(ns)
      y.label <- paste(y.varname, " (Mean +/- 95% CI)", sep = "")
      
    } else if (error.bars == "z.ci") {
      
      ns <- tapply(X = y, INDEX = group, FUN = length)
      sds <- tapply(X = y, INDEX = group, FUN = sd)
      z.crit <- qnorm(p = 1 - alpha/2)
      lower.bars <- means - z.crit * sds / sqrt(ns)
      upper.bars <- means + z.crit * sds / sqrt(ns)
      y.label <- paste(y.varname, " (Mean +/- 95% CI)", sep = "")
      
    }
    
    if (error.bars == "none") {
      
      lower.bars <- NULL
      upper.bars <- NULL
      
    }
    
    # Figure out ylim values
    max.error <- max(upper.bars)
    min.error <- min(lower.bars)
    span.error <- max.error - min.error
    y1 <- min.error - 0.1 * span.error
    y2 <- max.error + 0.1 * span.error
    
    # Figure out features of graph, based on user inputs where available
    plot.list <- list.override(list1 = list(x = means, type = "p", pch = 16, xaxt = "n",
                                            main = paste("Mean ", y.varname, " by ", group.varname, sep = ""),
                                            cex.main = 1.25,
                                            xlab = group.varname, ylab = y.label,
                                            xlim = c(0.5, length(group.levels) + 0.5),
                                            ylim = c(y1, y2)),
                               list2 = plot.list)
    axis.list <- list.override(list1 = list(side = 1, at = 1: length(group.levels), labels = group.levels),
                               list2 = axis.list)
    
    # Create graph
    do.call(plot, plot.list)
    
    # Add error bars
    for (ii in 1:length(lower.bars)) {
      
      end.points <- c(lower.bars[ii], upper.bars[ii])
      do.call(lines, c(list(x = rep(ii, 2), y = end.points), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03), y = rep(end.points[1], 2)), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03), y = rep(end.points[2], 2)), lines.list))
      
    }
    
    # Add labels
    do.call(axis, axis.list)
    
  }
  
}


# interval.groups(x = rnorm(100))
# 
# x <- c(rnorm(100), NA, NA, rnorm(100))
# y <- 0.5 + 1.25 * x + rnorm(202)
# group <- cut(x, breaks = quantile(x, na.rm = T), include.lowest = T)
# 
# means.graph(y = y, group = group)
