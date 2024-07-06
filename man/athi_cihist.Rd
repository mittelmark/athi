\name{athi$cihist} 
\alias{athi$cihist} 
\alias{athi_cihist} 
\title{Draw a histogram density line with confidence level} 
\description{ 
   This is a utility function to draw a histogram with density  
  lines and the confidence level for that density line. 
} 
\usage{ athi_cihist(x, conf.level=0.95, legend=TRUE, xlab="Value", ylab="Density", ...) } 
\arguments{ 
  \item{x}{ 
    a numerical vector 
  } 
  \item{conf.level}{ 
    The confidence level for the histogram, default 0.95 
  } 
  \item{legend}{ 
    Should a legend been drawn, default: TRUE 
  } 
  \item{xlab}{ 
    Label for the x-axis, default: "Value"    
  } 
  \item{ylab}{ 
    Label for the y-axis, default: "Density"    
  } 
  \item{\ldots}{ 
    Arguments delegated to the plot function 
  } 
} 
\examples{ 
  set.seed(123) 
  data <- rnorm(1000, mean = 0, sd = 1) 
  athi$cihist(data,main="Histogram density line with 95\% confidence level") 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
