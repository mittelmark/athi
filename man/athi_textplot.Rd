\name{athi$textplot} 
\alias{athi$textplot} 
\alias{athi_textplot} 
\title{Write the data for a data frame or matrix into a plot.} 
\description{This function can be used as a workaround to display data for small 
  data frames or matrices with let's say 3-8 rows or columns into a plot.  
} 
\usage{ athi_textplot(x,cex=1,caption=NULL,...) } 
\arguments{ 
\item{x}{data frame or matrix} 
\item{cex}{character expansion, default: 1} 
\item{caption}{optional caption shown below of the table, default: NULL} 
\item{\ldots}{remaining arguments delegated to the plot function} 
} 
\value{NULL} 
\examples{ 
par(mai=rep(0.1,4)) 
athi$textplot(head(swiss),caption="Table 1: Swiss data first six lines") 
} 
\seealso{ 
  \link[athi:athi-class]{athi-class} 
} 
 
