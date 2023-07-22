\name{athi$cohensD} 
\alias{athi$cohensD} 
\alias{athi_cohensD} 
\title{ Effect sizes measure for difference between two means } 
\description{ 
    Calculate the effect size for the difference between two means, divided by the pooled standard deviation. 
} 
\usage{ athi_cohensD(x,g,paired=FALSE) } 
\arguments{ 
 \item{x}{vector with numerical values} 
 \item{g}{vector with two grouping variables, having the same length as num} 
 \item{paired}{are the data paired, default: FALSE} 
} 
\details{ 
 The function cohensD calculates the effect size for the difference between two means. 
  Due to Cohen's rule of thumb values of around 0.2 are considered to stand  
  for small effects, values of around 0.5 represent medium effects and values of around 0.8  
  and larger represent large effects.  
 
Please note that these rules of thumb are not useful for highly dependent  
 outcome variables (death for instance) these rules might not be useful and as  
 well lower values might be of practical relevance. 
} 
\value{numerical value} 
\examples{ 
cohensD=athi$cohensD 
set.seed(125) 
data(sleep) 
with(sleep,cohensD(extra,group)) 
x1=rnorm(100,mean=20,sd=1) 
x2=rnorm(100,mean=22,sd=1) 
g1=rep('A',100) 
g2=rep('B',100) 
# difference should be around 2SD 
cohensD(c(x1,x2),as.factor(c(g1,g2))) 
# biseriell correlation coefficient as alternative 
# value is as well large 
cor(c(x1,x2),as.numeric(as.factor(c(g1,g2)))) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
