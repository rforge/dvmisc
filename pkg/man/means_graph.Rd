\name{means.graph}
\alias{means.graph}
\title{
Graph Means Across a Grouping Variable
}
\description{
Creates plot showing mean of Y variable across levels of a grouping variable, with customizable error bars.
}
\usage{
means.graph(y, group, error.bars = "t.ci", alpha = 0.05, 
            p.legend = TRUE, 
            plot.list = NULL, 
            lines.list = NULL,
            axis.list = NULL,
            legend.list = NULL, 
            ...)
}
\arguments{
  \item{y}{
Numeric vector of values for the continuous variable.
}
  \item{group}{
Vector of values indicating what group each y observation belongs to. Function plots group levels across x-axis in same order as table(group).
}
  \item{error.bars}{
Character string indicating what the error bars should represent. Possible values are "sd" for +/- one standard deviation, "se" for +/- one standard error, "t.ci" for 95\% confidence interval based on t distribution, "z.ci" for 95\% confidence interval based on z distribution, and "none" for no error bars.
}
  \item{alpha}{
Numeric value indicating what alpha should be set to for confidence intervals. Only used if error.bars is "t.ci" or "z.ci".   
}
  \item{p.legend}{
If TRUE, p-value is printed in a legend.  
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
  \item{legend.list}{
Optional list of inputs to pass to legend function.
}
  \item{...}{
Further arguments passed to other methods.
}
}
\details{
Function drops observations with missing values for y and/or group.
}
\value{
Plot showing mean of y across levels of group.
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