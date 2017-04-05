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
            plot.list = NULL, 
            lines.list = NULL,
            axis.list = NULL)
}
\arguments{
  \item{y}{
Numeric vector of values for the continuous variable.
}
  \item{group}{
Factor variable indicating what group each y observation belongs to.
}
  \item{error.bars}{
Character string indicating what the error bars should represent. Possible values are "sd" for +/- one standard deviation, "se" for +/- one standard error, "t.ci" for 95\% confidence interval based on t distribution, "z.ci" for 95\% confidence interval based on z distribution, and "none" for no error bars.
}
  \item{alpha}{
Numeric value indicating what alpha should be set to for confidence intervals. Only used if error.bars is "t.ci" or "z.ci".   
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
}
\details{
Both x and y can have missing values. The function drops observations with missing x or y.
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