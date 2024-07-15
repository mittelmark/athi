\name{athi$skewness} 
\alias{athi$skewness} 
\alias{athi_skewness} 
\title{Calculate the third central moment of a distribution} 
\usage{athi_skewness(x,na.rm=FALSE)}  
 
\description{ 
This function calculates the third central moment of a distribution. 
Values higher than zero indicate right-tailed distributions,  
values of lower than zero mean left-tailed distributions. 
Values around zero mean normal value like distribution.  
} 
\arguments{ 
\item{x}{vector with positive numerical values} 
\item{na.rm}{should NA's be removed, default: FALSE} 
} 
\value{numerical value for the skewness of the vector} 
\examples{ 
athi$skewness(1:100)          # very uniform, 0 
athi$skewness(rnorm(100))     # normal, close to 0 
## now with right tail 
athi$skewness(c(rnorm(100,mean=20),rnorm(30,mean=23,sd=2))) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_kurtosis]{athi$kurtosis} 
} 
 
