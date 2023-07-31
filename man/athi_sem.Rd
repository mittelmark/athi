\name{athi$sem} 
\alias{athi$sem} 
\alias{athi_sem} 
\title{Calculate the standard error of the mean for a given numerical vector.} 
\description{This function calculates the standard error of the mean, SEM = sd/sqrt(N).} 
\usage{ athi_sem(x,na.rm=FALSE) } 
\arguments{ 
\item{x}{a numerical vector} 
\item{na.rm}{logical vecor indicating if missing values should be removed, default: FALSE} 
} 
\value{computed standard error of the mean} 
\examples{ 
sem=athi$sem 
sem(rnorm(50,mean=10,sd=3)) 
sem(rnorm(1000,mean=10,sd=3)) 
} 
 
