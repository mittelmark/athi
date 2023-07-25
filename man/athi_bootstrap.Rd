\name{athi$bootstrap} 
\alias{athi$bootstrap} 
\alias{athi_bootstrap} 
\title{ Perform a resampling for the given data set and function. } 
\description{ 
  The function allows you to perform a resampling method without replacement to perform 
  a boostrap analysis for instance to cmpute a p-value or a confidence interval. 
} 
\usage{ athi_bootstrap(x,FUN=NULL,n=1000,...) } 
\arguments{ 
   \item{x}{a vector, a data frame or a matrix} 
   \item{FUN}{function handling the given data set type but performing before executing  FUN a sampling with replacement, please note that the function must return a scalar value} 
   \item{n}{ number of resamplings to perform, default: 100} 
   \item{\ldots}{remaining arguments are delegated to the given function `FUN`} 
} 
\value{vector with the resampled values from the given function} 
\examples{ 
  rn=rnorm(100,mean=10,sd=2) 
  t.test(rn)$conf.int 
  vals=athi$bootstrap(rn,FUN=mean) 
  summary(vals) 
  quantile(vals,c(0.025,0.975)) # 95% CI is very close 
  # confidence interval for spearman correlation 
  cor.test(swiss[,1],swiss[,2],method="spearman") 
  vals=athi$bootstrap(swiss[,c(1,2)], 
       FUN=function(x) cor(x[,1],x[,2],method="spearman")) 
  summary(vals) 
  quantile(vals,c(0.025,0.975)) # 95% CI shows insignifcant 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class} 
} 
 
 
