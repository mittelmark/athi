\name{athi$kurtosis} 
\alias{athi$kurtosis} 
\alias{athi_kurtosis} 
\title{Calculate the fourth central moment of a distribution} 
\usage{athi_kurtosis(x,na.rm=FALSE)} 
\description{ 
The function calculates the fourth central moment of a distribution. 
Values higher than 0 
indicate heavy-tailed distributions, values of lower than zero means  
light-tailed (sharp peak) distributions. Values around zero mean normal value 
like distribution. As the normal kurtosis formula has for normal distributions 
a value of three, usually the excess kurtosis as in this implementation is  
used which involves substraction of 3. 
} 
 
\arguments{ 
\item{x}{vector with positive numerical values} 
\item{na.rm}{should NA's be removed, default: FALSE} 
} 
\value{numerical value for the excess kurtosis} 
\examples{ 
athi$kurtosis(1:10)      # very uniform, should be negative 
athi$kurtosis(runif(10,min=1,max=5)+rnorm(10,mean=3,sd=0.2)) 
athi$kurtosis(rnorm(100,mean=10,sd=0.5)) # close to zero 
athi$kurtosis(rt(50,df=1)) # higher than normal 
} 
 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_skewness]{athi$skewness} 
} 
 
