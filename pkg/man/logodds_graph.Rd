\name{logodds.graph}
\alias{logodds.graph}
\title{
Graph Log-Odds of Binary Variable Across A Grouping Variable
}
\description{
Creates plot showing sample log-odds of binary Y variable across levels of a grouping variable, with customizable error bars.
}
\usage{
logodds.graph(y, group, error.bars = "none", alpha = 0.05,
              p.legend = "chi",
              plot.list = NULL, 
              lines.list = NULL,
              axis.list = NULL,
              ...)
}
\arguments{
  \item{y}{
Vector of values for binary response variable. Must take on 2 values, but can be any type (e.g. numeric, character, factor, logical). Function plots log-odds of second value returned by table(y).
}
  \item{group}{
Vector of values indicating what group each y observation belongs to. Function plots group levels across x-axis in same order as table(group).
}
  \item{error.bars}{
Character string indicating what the error bars should represent. Possible values are "exact.ci" for exact 95\% confidence interval based on binomial distribution, "z.ci" for approximate 95\% confidence interval based on z distribution, and "none" for no error bars.
}
  \item{alpha}{
Numeric value indicating what alpha should be set to for confidence intervals. Only used if error.bars is "exact.ci" or "z.ci".   
}
  \item{p.legend}{
Character string controlling what p-value is printed in a legend. Possible values are "exact" for exact binomial test
If TRUE, p-value  from binom.test or (from t.test function if group has 2 levels, otherwise aov function) is printed in a legend.
}
  \item{plot.list}{
Optional list of inputs to pass to plot function.  
}
  \item{lines.list}{
Optional list of inputs to pass to lines function.  
}
  \item{axis.list}{
Optional list of inputs to pass to axis function.
}
  \item{...}{
Further arguments passed to other methods.
}
}
\details{
Function drops observations with missing values for y and/or group.
}
\value{
Plot showing log-odds of y across levels of group.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
NA
}
\seealso{
NA
}
\examples{
NA
}