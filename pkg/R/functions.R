# Adds elements of list2 to list1, overwriting any elements with the same name
list.override <- function(list1, list2) {

  # Get names of elements of list1 and list2
  names.list1 <- names(list1)
  names.list2 <- names(list2)

  # Loop through elements of list 2. If in list 1, remove, then add; if not in
  # list 1, add.
  for (ii in 1: length(list2)) {

    element.name <- names.list2[ii]
    loc.list1 <- which(names.list1 == element.name)
    if (length(loc.list1) > 0) {
      list1[loc.list1] <- list2[ii]
    } else {
      list1 <- c(list1, list2[ii])
    }

  }

  # Return list1, which has its original elements plus any extras/overrides from
  # list2
  return(list1)

}


# Check if numeric value is in between two other values
inside <- function(x, ends, inclusive = TRUE) {

  if (inclusive) {
    x >= ends[1] & x <= ends[2]
  } else {
    x > ends[1] & x < ends[2]
  }

}


# Create fixed number of groups covering range of input vector x
interval.groups <- function(x, groups = 5, ...) {

  # Figure out break points to split x into even intervals spanning its range
  x.range <- range(x, na.rm = TRUE)
  cut.breaks <- seq(x.range[1], x.range[2], diff(x.range) / groups)
  cut.breaks[c(1, length(cut.breaks))] <- c(-Inf, Inf)

  # Create groups
  groups <- cut(x = x, breaks = cut.breaks, ...)

  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ",
                paste(table(groups), collapse = ", "),
                ". ", num.missing, " missing.",
                sep = ""))
  return(groups)

}


# Create quantile groups. Consider adding labels option, e.g. could be "#" for
# number, "Q#" for Q1, Q2, or "interval" for the actual intervals.
quant.groups <- function(x, groups = 5, ...) {

  # Calculate quantiles
  quantiles <- quantile(x, probs = seq(0, 1, 1 / groups), na.rm = TRUE, ...)

  # Create quantile groups
  groups <- cut(x, breaks = quantiles, include.lowest = TRUE, ...)

  # Print message and return groups
  num.missing <- sum(is.na(groups))
  message(paste("Observations per group: ",
                paste(table(groups), collapse = ", "),
                ". ", num.missing, " missing.",
                sep = ""))
  return(groups)

}


# Create 3 BMI groups
bmi3 <- function(x, labels = TRUE) {
  if (labels) {
    y <- cut(x, breaks = c(-Inf, 25, 30, Inf), right = F,
             labels = c("Normal weight", "Overweight", "Obese"))
  } else {
    y <- cut(x, breaks = c(-Inf, 25, 30, Inf), right = F)
  }
  return(y)
}


# Create 4 BMI groups
bmi4 <- function(x, labels = TRUE) {
  if (labels) {
    y <- cut(x, breaks = c(-Inf, 18.5, 25, 30, Inf), right = F,
             labels = c("Underweight", "Normal weight", "Overweight", "Obese"))
  } else {
    y <- cut(x, breaks = c(-Inf, 18.5, 25, 30, Inf), right = F)
  }
  return(y)
}


# Create graph of point +/- error bar
dots.bars <- function(y = NULL,
                      bars = NULL,
                      bars.lower = y - bars,
                      bars.upper = y + bars,
                      group.labels = NULL,
                      subgroup.labels = NULL,
                      subgroup.pch = NULL,
                      subgroup.col = NULL,
                      points.list = NULL,
                      arrows.list = NULL,
                      axis.list = NULL,
                      legend.list = NULL,
                      ...) {

  if (! is.matrix(y) | (is.matrix(y) && (ncol(y) == 1 | nrow(y) == 1))) {

    # Code to execute if there are no subgroups

    # Create x-values for plot
    xvals <- 1: length(y)

    # If NULL, assign generic values to group.labels
    if (is.null(group.labels)) {
      if (is.null(names(y))) {
        group.labels <- xvals
      } else {
        group.labels <- names(y)
      }
    }

    # Create list of extra arguments
    extra.args <- list(...)

    # If NULL, figure out default values for various plot features
    if (is.null(extra.args$ylab)) {
      extra.args$ylab = deparse(substitute(y))
    }
    if (is.null(extra.args$xlab)) {
      extra.args$xlab = "Group"
    }
    if (is.null(extra.args$main)) {
      extra.args$main <- paste(extra.args$ylab, " by ", extra.args$xlab, sep = "")
    }
    if (is.null(extra.args$ylim)) {
      yrange <- max(bars.upper) - min(bars.lower)
      extra.args$ylim <- c(min(bars.lower) - 0.05 * yrange,
                           max(bars.upper) + 0.05 * yrange)
      if (all(extra.args$ylim > 0)) {
        extra.args$ylim[1] <- 0
      } else if (all(extra.args$ylim < 0)) {
        extra.args$ylim[2] <- 0
      }
    }
    if (is.null(extra.args$xlim)) {
      extra.args$xlim <- c(min(xvals) - 0.75, max(xvals) + 0.75)
    }

    # Create plot
    do.call(plot, c(list(x = xvals, y = y, xaxt = "n", type = "n"),
                    extra.args))

    # Add points and error bars
    do.call(points, c(list(x = xvals, y = y), points.list))
    arrows.list <- list.override(list1 = list(length = 0.05, angle = 90,
                                              code = 3),
                                 list2 = arrows.list)
    do.call(arrows, c(list(x0 = xvals, y0 = bars.lower,
                           x1 = xvals, y1 = bars.upper),
                      arrows.list))

    # Add group labels on x-axis
    axis.list <- list.override(list1 = list(side = 1, at = xvals,
                                            labels = group.labels),
                               list2 = axis.list)
    do.call(axis, axis.list)

  } else {

    # Code to execute if there are subgroups

    # Get number of groups and number of subgroups within each group
    group.n <- ncol(y)
    subgroup.n <- nrow(y)

    # Create x-values for plot
    xvals <- 1: group.n

    # If NULL, assign generic values to group.labels, subgroup.labels, and
    # subgroup.pch
    if (is.null(group.labels)) {
      if (is.null(colnames(y))) {
        group.labels <- xvals
      } else {
        group.labels <- colnames(y)
      }
    }
    if (is.null(subgroup.labels)) {
      if (is.null(rownames(y))) {
        subgroup.labels <- LETTERS[1: subgroup.n]
      } else {
        subgroup.labels <- rownames(y)
      }
    }
    if (is.null(subgroup.pch)) {
      if (subgroup.n <= 5) {
        subgroup.pch <- c(1, 18, 8, 0, 4)[1: subgroup.n]
      } else {
        subgroup.pch <- 1: subgroup.n
      }
    }
    if (is.null(subgroup.col)) {
      subgroup.col <- rep("black", subgroup.n)
    }

    # Create list of extra arguments
    extra.args <- list(...)

    # If NULL, figure out default values for various plot features
    if (is.null(extra.args$ylab)) {
      extra.args$ylab = deparse(substitute(y))
    }
    if (is.null(extra.args$xlab)) {
      extra.args$xlab = "Group"
    }
    if (is.null(extra.args$main)) {
      extra.args$main <- paste(extra.args$ylab, " by ", extra.args$xlab, sep = "")
    }
    if (is.null(extra.args$ylim)) {
      yrange <- max(bars.upper) - min(bars.lower)
      extra.args$ylim <- c(min(bars.lower) - 0.05 * yrange,
                           max(bars.upper) + 0.05 * yrange)
      if (all(extra.args$ylim > 0)) {
        extra.args$ylim[1] <- 0
      } else if (all(extra.args$ylim < 0)) {
        extra.args$ylim[2] <- 0
      }
    }
    if (is.null(extra.args$xlim)) {
      extra.args$xlim <- c(min(xvals) - 0.75, max(xvals) + 0.75)
    }

    # Create plot
    do.call(plot, c(list(cbind(xvals, t(y)), xaxt = "n", type = "n"),
                    extra.args))

    # Create x.steps vector to offset subgroups
    x.steps <- seq(-0.15, 0.15, 0.3 / (subgroup.n - 1))

    # Loop through and add points and bars
    arrows.list <- list.override(list1 = list(length = 0.05, angle = 90,
                                              code = 3),
                                 list2 = arrows.list)
    for (ii in 1: subgroup.n) {
      do.call(points, c(list(x = xvals + x.steps[ii], y = y[ii, ],
                             pch = subgroup.pch[ii], col = subgroup.col[ii]),
                        points.list))
      do.call(arrows, c(list(x0 = xvals + x.steps[ii], y0 = bars.lower[ii, ],
                             x1 = xvals + x.steps[ii], y1 = bars.upper[ii, ]),
                        arrows.list))
    }

    # Add group labels on x-axis
    axis.list <- list.override(list1 = list(side = 1, at = xvals,
                                            labels = group.labels),
                               list2 = axis.list)
    do.call(axis, axis.list)

    # Add legend
    legend.list <- list.override(list1 = list(x = "bottomleft",
                                              pch = subgroup.pch,
                                              legend = subgroup.labels),
                                 list2 = legend.list)
    do.call(legend, legend.list)

  }
}


# Create graph of mean +/- error bar for continuous variable vs. factor
means.graph <- function(y, group, error.bars = "t.ci", alpha = 0.05,
                        p.legend = TRUE,
                        plot.list = NULL,
                        lines.list = NULL,
                        axis.list = NULL,
                        legend.list = NULL,
                        ...) {

  # Get name of y and group variables for axis labels
  y.varname <- deparse(substitute(y))
  group.varname <- deparse(substitute(group))

  # Drop missing values
  locs.missing <- which(is.na(y) | is.na(group))
  if (length(locs.missing) > 0) {
    y <- y[-locs.missing]
    group <- group[-locs.missing]
  }

  # Get levels of groups variable for tick labels
  group.levels <- names(table(group))

  # Get group means
  means <- tapply(X = y, INDEX = group, FUN = mean)

  # Create error bars
  if (error.bars == "sd") {

    sds <- tapply(X = y, INDEX = group, FUN = sd)
    lower.bars <- means - sds
    upper.bars <- means + sds
    y.label <- paste(y.varname, " (Mean +/- 1 SD)", sep = "")

  } else if (error.bars == "se") {

    ses <- tapply(X = y, INDEX = group, FUN = function(x)
      sd(x) / sqrt(length(x)))
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

  } else if (error.bars == "none") {

    lower.bars <- NULL
    upper.bars <- NULL
    y.label <- paste(y.varname, " (Mean)", sep = "")

  }

  # Figure out ylim values
  if (!is.null(lower.bars)) {
    max.error <- max(upper.bars)
    min.error <- min(lower.bars)
    span.error <- max.error - min.error
    y1 <- min.error - 0.1 * span.error
    y2 <- max.error + 0.1 * span.error
  } else {
    range.means <- range(means)
    span.means <- diff(range.means)
    y1 <- range.means[1] - 0.1 * span.means
    y2 <- range.means[2] + 0.1 * span.means
  }

  # Figure out features of graph, based on user inputs where available
  plot.list <-
    list.override(list1 = list(x = means, type = "p", pch = 16, xaxt = "n",
                               main = paste("Mean ", y.varname, " by ",
                                            group.varname, sep = ""),
                               cex.main = 1.25,
                               xlab = group.varname, ylab = y.label,
                               xlim = c(0.5, length(group.levels) + 0.5),
                               ylim = c(y1, y2)),
                             list2 = plot.list)
  axis.list <-
    list.override(list1 = list(side = 1, at = 1: length(group.levels),
                               labels = group.levels),
                  list2 = axis.list)

  # Create graph
  do.call(plot, plot.list)

  # Add error bars
  if (error.bars != "none") {
    for (ii in 1:length(lower.bars)) {

      end.points <- c(lower.bars[ii], upper.bars[ii])
      do.call(lines, c(list(x = rep(ii, 2), y = end.points), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03),
                            y = rep(end.points[1], 2)), lines.list))
      do.call(lines, c(list(x = c(ii - 0.03, ii + 0.03),
                            y = rep(end.points[2], 2)), lines.list))

    }
  }

  # Add legend
  if (p.legend) {

    # Perform t-test/ANOVA
    if (length(unique(group)) == 2) {
      pval <- t.test(y ~ group, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "T-test P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("T-test P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("T-test P = ", sprintf("%.2f", pval), sep = "")
      }
    } else {
      pval <- summary(aov(y ~ group, ...))[[1]][[1, "Pr(>F)"]]
      if (pval < 0.001) {
        pval.text <- "ANOVA P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("ANOVA P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("ANOVA P = ", sprintf("%.2f", pval), sep = "")
      }
    }

    # Add user inputs to legend, if any
    legend.list <- list.override(list1 = list(x = "topleft",
                                              legend = pval.text),
                                 list2 = legend.list)

    # Add legend
    do.call(legend, legend.list)

  }

  # Add labels
  do.call(axis, axis.list)

}


# Plot sample log-odds for binary variable vs. factor
logodds.graph <- function(y, group, error.bars = "none", alpha = 0.05,
                          p.legend = "chi",
                          plot.list = NULL,
                          lines.list = NULL,
                          axis.list = NULL,
                          legend.list = NULL,
                          ...) {

  # Get name of y and group variables for axis labels
  y.varname <- deparse(substitute(y))
  group.varname <- deparse(substitute(group))

  # Drop missing values
  locs.missing <- which(is.na(y) | is.na(group))
  if (length(locs.missing) > 0) {
    y <- y[-locs.missing]
    group <- group[-locs.missing]
  }

  # Create contingency table
  group.y.table <- table(group, y)
  x <- 1: nrow(group.y.table)

  # Get levels of groups variable for tick labels
  group.levels <- rownames(group.y.table)

  # Exclude rows of 0
  locs.0 <- which(apply(group.y.table, 1, sum) == 0)
  if (length(locs.0) > 0) {
    group.y.table <- group.y.table[-locs.0, ]
    x <- x[-locs.0]
  }

  # Get outcome probabilities and log-odds for each level
  probs <- apply(group.y.table, 1, function(x) x[2] / sum(x))
  logodds <- log(probs / (1 - probs))

  # Create error bars
  if (error.bars == "exact.ci") {

    probs.ci <- apply(group.y.table, 1, function(x)
      binom.test(x = rev(x), conf.level = 1 - alpha)$conf.int)
    logodds.ci <- log(probs.ci / (1 - probs.ci))
    lower.bars <- logodds.ci[1, ]
    upper.bars <- logodds.ci[2, ]
    y.label <- paste(y.varname, " (log-odds +/- 95% CI)", sep = "")

  } else if (error.bars == "z.ci") {

    probs.ci <- apply(group.y.table, 1, function(x)
      prop.test(x = x[2], n = sum(x), conf.level = 1 - alpha)$conf.int)
    logodds.ci <- log(probs.ci / (1 - probs.ci))
    lower.bars <- logodds.ci[1, ]
    upper.bars <- logodds.ci[2, ]
    y.label <- paste(y.varname, " (log-odds +/- 95% CI)", sep = "")

  } else if (error.bars == "none") {

    lower.bars <- NULL
    upper.bars <- NULL
    y.label <- paste(y.varname, " (log-odds)", sep = "")

  }

  # Figure out ylim values
  if (!is.null(lower.bars)) {
    max.error <- max(upper.bars[!is.infinite(upper.bars)])
    min.error <- min(lower.bars[!is.infinite(lower.bars)])
    span.error <- max.error - min.error
    y1 <- min.error - 0.1 * span.error
    y2 <- max.error + 0.1 * span.error
    upper.bars[upper.bars == Inf] <- max.error + span.error
    lower.bars[lower.bars == -Inf] <- min.error - span.error
  } else {
    range.logodds <- range(logodds[!is.infinite(logodds)])
    span.logodds <- diff(range.logodds)
    y1 <- range.logodds[1] - 0.1 * span.logodds
    y2 <- range.logodds[2] + 0.1 * span.logodds
  }

  # Figure out features of graph, based on user inputs where available
  plot.list <-
    list.override(list1 = list(x = x, y = logodds,
                               type = "p", pch = 16, xaxt = "n",
                               main = paste("Log-odds ", y.varname, " by ",
                                            group.varname, sep = ""),
                               cex.main = 1.25,
                               xlab = group.varname, ylab = y.label,
                               xlim = c(0.5, length(group.levels) + 0.5),
                               ylim = c(y1, y2)),
                  list2 = plot.list)
  cex.axis.value <- ifelse(length(group.levels) <= 3, 1,
                           ifelse(length(group.levels) >= 8, 0.5,
                                  1 - 0.1 * (length(group.levels) - 3)))
  axis.list <-
    list.override(list1 = list(side = 1, at = 1: length(group.levels),
                               labels = group.levels,
                               cex.axis = cex.axis.value),
                  list2 = axis.list)

  # Create graph
  do.call(plot, plot.list)

  # Add error bars
  if (error.bars != "none") {
    for (ii in 1:length(lower.bars)) {

      end.points <- c(lower.bars[ii], upper.bars[ii])
      do.call(lines, c(list(x = rep(x[ii], 2), y = end.points), lines.list))
      do.call(lines, c(list(x = c(x[ii] - 0.03, x[ii] + 0.03),
                            y = rep(end.points[1], 2)), lines.list))
      do.call(lines, c(list(x = c(x[ii] - 0.03, x[ii] + 0.03),
                            y = rep(end.points[2], 2)), lines.list))

    }
  }

  # Add labels
  do.call(axis, axis.list)

  # Add legend
  if (p.legend != "none") {

    if (p.legend == "chi") {

      # Perform Chi-square test for association
      pval <- chisq.test(x = group, y = y, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "Chi-square P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("Chi-square P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("Chi-square P = ", sprintf("%.2f", pval), sep = "")
      }

    } else if (p.legend == "fisher") {

      # Perform Fisher's exact test
      pval <- fisher.test(x = group, y = y, ...)$p.value
      if (pval < 0.001) {
        pval.text <- "Fisher P < 0.001"
      } else if (pval < 0.05) {
        pval.text <- paste("Fisher P = ", sprintf("%.3f", pval), sep = "")
      } else {
        pval.text <- paste("Fisher P = ", sprintf("%.2f", pval), sep = "")
      }

    }

    # Add user inputs to legend, if any
    legend.list <-
      list.override(list1 = list(x = "topleft", legend = pval.text),
                    list2 = legend.list)

    # Add legend
    do.call(legend, legend.list)

  }

}


# Extract mean squared error from lm or glm model
get.mse <- function(model.fit, var.estimate = FALSE) {

  # Extract MSE from model.fit
  class_model.fit <- class(model.fit)[1]
  if (class_model.fit == "lm") {
    mse <- rev(anova(model.fit)$"Mean Sq")[1]
  } else if (class_model.fit == "glm") {
    mse <- sum(model.fit$residuals^2) / model.fit$df.residual
  }

  # Get variance estimate for error variance if requested
  if (var.estimate) {
    mse.var <- 2 * mse^2 / model.fit$df.residual
    mse <- c(mse.hat = mse, mse.var = mse.var)
  }

  # Return mse variable
  return(mse)

}


# Histogram with some added features
# Think about adding some transformation options... Could just leave it to
# the user, but might be nice to add something like "boxcox.norm",
# "sqrt.norm", "inverse.norm", etc.
histo <- function(x,
                  dis = "none", dis.shift = NULL,
                  integer.breaks = NULL,
                  points.list = NULL,
                  axis.list = NULL,
                  ...) {

  # Create list with ... arguments
  extra.args <- list(...)

  # Extract any parameters (i.e. arguments NOT for hist) included in ...
  if (!is.null(extra.args)) {
    loc <- which(names(extra.args) == "size")
    if (length(loc) == 1) {
      size <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "N")
    if (length(loc) == 1) {
      N <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
    loc <- which(names(extra.args) == "k")
    if (length(loc) == 1) {
      k <- extra.args[[loc]]
      extra.args <- extra.args[-loc]
    }
  }

  # If integer.breaks is NULL, set to TRUE if x takes on whole numbers with 20
  # or fewer distinct values, else set to FALSE
  if (is.null(integer.breaks)) {
    integer.breaks <- all(x %% 1 == 0) & length(unique(x)) <= 10
  }

  # If right is not specified or integer.breaks is TRUE, set to TRUE
  if (! "right" %in% names(extra.args) | integer.breaks) {
    extra.args$right <- TRUE
  }

  # If integer.breaks is TRUE, make breaks a vector of integers
  # covering the range of x
  if (integer.breaks) {
    extra.args$breaks <- seq(floor(min(x)) - 1, ceiling(max(x)), 1)
  }

  # If freq is not specified, set to FALSE
  if (! "freq" %in% names(extra.args)) {
    extra.args$freq <- FALSE
  }

  # If xlab/main not specified, set
  if (! "xlab" %in% names(extra.args)) {
    extra.args$xlab <- deparse(substitute(x))
  }
  if (! "main" %in% names(extra.args)) {
    extra.args$main <- paste("Histogram of ", deparse(substitute(x)), sep = "")
  }

  # Create histogram
  if (integer.breaks) {
    hist.fig <- do.call(hist, c(list(x = quote(x), xaxt = "n"), extra.args))
    hist.fig <- do.call(axis, c(list(side = 1, at = hist.fig$mids,
                                     labels = hist.fig$breaks[-1]),
                                axis.list))
  } else {
    hist.fig <- do.call(hist, c(list(x = quote(x)), extra.args))
  }

  # Add fitted pdf/pmf if requested
  if (dis != "none") {

    if (dis == "beta") {

      theta.hat <- fitdistr(x, "beta", start = list(shape1 = 0.5,
                                                    shape2 = 0.5))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dbeta(x = x.vals, shape1 = theta.hat[1], shape2 = theta.hat[2])

    } else if (dis == "binom") {

      # Need user-input value for size
      p.hat <- mean(x) / size
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dbinom(x = x.vals, size = size, prob = p.hat)

    } else if (dis == "cauchy") {

      theta.hat <- fitdistr(x, "cauchy")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <-
        dcauchy(x = x.vals, location = theta.hat[1], scale = theta.hat[2])
      do.call(points, c(list(x = x.vals, y = y.vals, type = "l"), points.list))

    } else if (dis == "chisq") {

      theta.hat <- fitdistr(x, "chi-squared", start = list(df = 1))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dchisq(x = x.vals, df = theta.hat[1])

    } else if (dis == "exp") {

      theta.hat <- fitdistr(x, "exponential")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dexp(x = x.vals, rate = theta.hat)

    } else if (dis == "f") {

      theta.hat <- fitdistr(x, "f", start = list(df1 = 1, df2 = 2))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- df(x = x.vals, df1 = theta.hat[1], df2 = theta.hat[2])

    } else if (dis == "gamma") {

      theta.hat <- fitdistr(x, "gamma",
                            start = list(shape = 1, scale = 1))$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dgamma(x = x.vals, shape = theta.hat[1], scale = theta.hat[2])
      do.call(points, c(list(x = x.vals, y = y.vals, type = "l"), points.list))

    } else if (dis == "geom") {

      theta.hat <- fitdistr(x, "geometric")$estimate
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dgeom(x = x.vals, prob = theta.hat)

    } else if (dis == "hyper") {

      # Need user-input values for N, k
      loglik.f.hyper <- function(m) {
        n <- N - m
        ll <- sum(log(choose(m, x) * choose(n, k - x) / choose(n + m, k)))
        return(-ll)
      }
      m.hat <- round(nlminb(objective = loglik.f.hyper, start = k)$par)
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dhyper(x = x.vals, m = m.hat, n = N - m.hat, k = k)

    } else if (dis == "lnorm") {

      theta.hat <- fitdistr(x, "lognormal")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dlnorm(x = x.vals, meanlog = theta.hat[1], sdlog = theta.hat[2])

    } else if (dis == "nbinom") {

      loglik.f.nbinom <- function(p) {
        ll <- sum(log(gamma(x + size) / (gamma(size) * factorial(x)) *
                        p^size * (1 - p)^x))
        return(-ll)
      }
      p.hat <- nlminb(objective = loglik.f.nbinom, start = 0.5)$par
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dnbinom(x = x.vals, size = size, prob = p.hat)

    } else if (dis == "norm") {

      theta.hat <- fitdistr(x, "normal")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dnorm(x = x.vals, mean = theta.hat[1], sd = theta.hat[2])

    } else if (dis == "pois") {

      theta.hat <- fitdistr(x, "poisson")$estimate
      x.vals <- seq(round(min(x)), round(max(x)), 1)
      y.vals <- dpois(x = x.vals, lambda = theta.hat)

    } else if (dis == "t") {

      theta.hat <- fitdistr(x, "t")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dt(x = x.vals, df = theta.hat[3])

    } else if (dis == "unif") {

      min.hat <- min(x)
      max.hat <- max(x)
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dunif(x = x.vals, min = min.hat, max = max.hat)

    } else if (dis == "weibull") {

      theta.hat <- fitdistr(x, "weibull")$estimate
      x.vals <- seq(min(x), max(x), diff(range(x)) / 1000)
      y.vals <- dweibull(x = x.vals, shape = theta.hat[1], scale = theta.hat[2])

    }

    # If dis.shift is NULL and all values in x are integers, figure out how to
    # shift curve to make it match up with the histogram bars
    if (is.null(dis.shift)) {
      if (all(x %% 1 == 0)) {
        dis.shift <- ifelse(extra.args$right, -0.5, 0.5)
      } else {
        dis.shift <- 0
      }
    }

    # Add overlaying curve
    points.list <- list.override(list1 = list(type = "l"),
                                 list2 = points.list)
    do.call(points, c(list(x = x.vals + dis.shift, y = y.vals),
                      points.list))

  }

}


# Sum of integers
sum_i <- function(x) {
  .Call('dvmisc_sumc_i', PACKAGE = 'dvmisc', x)
}


# Mean of numeric values
mean_n <- function(x) {
  out <- sum(x) / length(x)
  return(out)
}


# Mean of integer values
mean_i <- function(x) {
  .Call('dvmisc_meanc_i', PACKAGE = 'dvmisc', x)
}


# Sample variance for numeric values
var_n <- function(x) {
  .Call('dvmisc_varc_n', PACKAGE = 'dvmisc', x)
}


# Sample variance for integer values
var_i <- function(x) {
  .Call('dvmisc_varc_i', PACKAGE = 'dvmisc', x)
}


# Sample covariance for numeric values
cov_n <- function(x, y) {
  .Call('dvmisc_covc_n', PACKAGE = 'dvmisc', x, y)
}


# Sample covariance for integer values
cov_i <- function(x, y) {
  .Call('dvmisc_covc_i', PACKAGE = 'dvmisc', x, y)
}


# Sample standard deviation for numeric values
sd_n <- function(x) {
  sqrt(.Call('dvmisc_varc_n', PACKAGE = 'dvmisc', x))
}


# Sample standard deviation for integer values
sd_i <- function(x) {
  sqrt(.Call('dvmisc_varc_i', PACKAGE = 'dvmisc', x))
}


# Minimum of numeric values
min_n <- function(x) {
  .Call('dvmisc_minc_n', PACKAGE = 'dvmisc', x)
}


# Minimum of integer values
min_i <- function(x) {
  .Call('dvmisc_minc_i', PACKAGE = 'dvmisc', x)
}


# Maximum of numeric values
max_n <- function(x) {
  .Call('dvmisc_maxc_n', PACKAGE = 'dvmisc', x)
}


# Maximum of integer values
max_i <- function(x) {
  .Call('dvmisc_maxc_i', PACKAGE = 'dvmisc', x)
}


# "Range" of numeric values
range_n <- function(x) {
  .Call('dvmisc_rangec_n', PACKAGE = 'dvmisc', x)
}


# "Range" of integer values
range_i <- function(x) {
  .Call('dvmisc_rangec_i', PACKAGE = 'dvmisc', x)
}


# True range of numeric values
true_range_n <- function(x) {
  .Call('dvmisc_true_rangec_n', PACKAGE = 'dvmisc', x)
}


# True range of integer values
true_range_i <- function(x) {
  .Call('dvmisc_true_rangec_i', PACKAGE = 'dvmisc', x)
}


# 1-unit diff for numeric values
diff1_n <- function(x) {
  .Call('dvmisc_diff1c_n', PACKAGE = 'dvmisc', x)
}


# 1-unit diff for integer values
diff1_i <- function(x) {
  .Call('dvmisc_diff1c_i', PACKAGE = 'dvmisc', x)
}


# diff for numeric values
diff_n <- function(x, lag = 1) {
  .Call('dvmisc_diffc_n', PACKAGE = 'dvmisc', x, lag)
}


# diff for integer values
diff_i <- function(x, lag = 1) {
  .Call('dvmisc_diffc_i', PACKAGE = 'dvmisc', x, lag)
}


# which.min for numeric vector
which_min_nv <- function(x) {
  .Call('dvmisc_which_minc_nv', PACKAGE = 'dvmisc', x)
}


# which.min for integer vector
which_min_iv <- function(x) {
  .Call('dvmisc_which_minc_iv', PACKAGE = 'dvmisc', x)
}


# which.min for numeric matrix
which_min_nm <- function(x) {
  .Call('dvmisc_which_minc_nm', PACKAGE = 'dvmisc', x)
}


# which.min for integer matrix
which_min_im <- function(x) {
  .Call('dvmisc_which_minc_im', PACKAGE = 'dvmisc', x)
}


# which.max for numeric vector
which_max_nv <- function(x) {
  .Call('dvmisc_which_maxc_nv', PACKAGE = 'dvmisc', x)
}


# which.max for integer vector
which_max_iv <- function(x) {
  .Call('dvmisc_which_maxc_iv', PACKAGE = 'dvmisc', x)
}


# which.max for numeric matrix
which_max_nm <- function(x) {
  .Call('dvmisc_which_maxc_nm', PACKAGE = 'dvmisc', x)
}


# which.max for integer matrix
which_max_im <- function(x) {
  .Call('dvmisc_which_maxc_im', PACKAGE = 'dvmisc', x)
}


# Weighted mean for numeric values with numeric weights
weighted_mean_nn <- function(x, w) {
  .Call('dvmisc_weighted_meanc_nn', PACKAGE = 'dvmisc', x, w)
}


# Weighted mean for numeric values with integer weights
weighted_mean_ni <- function(x, w) {
  .Call('dvmisc_weighted_meanc_ni', PACKAGE = 'dvmisc', x, w)
}


# Weighted mean for integer values with numeric weights
weighted_mean_in <- function(x, w) {
  .Call('dvmisc_weighted_meanc_in', PACKAGE = 'dvmisc', x, w)
}


# Weighted mean for integer values with integer weights
weighted_mean_ii <- function(x, w) {
  .Call('dvmisc_weighted_meanc_ii', PACKAGE = 'dvmisc', x, w)
}


# Pooled sample variance for numeric vectors
pooled_var_n <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  s2 <- ((n1 - 1) * .Call('dvmisc_varc_n', PACKAGE = 'dvmisc', x) +
           (n2 - 1) * .Call('dvmisc_varc_n', PACKAGE = 'dvmisc', y)) /
    (n1 + n2 - 2)
  return(s2)
}


# Pooled sample variance for integer vectors
pooled_var_i <- function(x, y) {
  n1 <- length(x)
  n2 <- length(y)
  s2 <- ((n1 - 1) * .Call('dvmisc_varc_i', PACKAGE = 'dvmisc', x) +
           (n2 - 1) * .Call('dvmisc_varc_i', PACKAGE = 'dvmisc', y)) /
    (n1 + n2 - 2)
  return(s2)
}


# Convert probability to odds
prob_odds <- function(x) {
  out <- x / (1 - x)
  return(out)
}


# Convert probability to logit
prob_logit <- function(x) {
  out <- log(x / (1 - x))
  return(out)
}


# Convert odds to probability
odds_prob <- function(x) {
  out <- x / (x + 1)
  return(out)
}


# Convert logit to probability
logit_prob <- function(x) {
  exp_x <- exp(x)
  out <- exp_x / (1 + exp_x)
  return(out)
}
