\name{athi$wilcoxR} 
\alias{athi$wilcoxR} 
\alias{athi_wilcoxR} 
\title{Calculate the effect size for a wilcox test (Rosenthal 1991).} 
\description{This function calculates the effect size for a numerical vector with non-normally distributed variables 
 and a categorical vector with two levels. 
} 
\usage{ athi_wilcoxR(x,y=NULL,n=NULL) } 
\arguments{ 
\item{x}{either a wilcox test object or a vector with numerical values} 
\item{y}{if _x_ is numerical vector either a vector with numerical values or a vector with cvategorical data having the same length as x} 
\item{n}{numnber of samples, required if _x_ is a wilcox.test object} 
} 
\value{numerical value for effect size r having the same interpretation rules a Pearson's r} 
\examples{ 
set.seed(123) 
rn1=rnorm(100,mean=10,sd=1) 
rn2=rnorm(80,mean=11,sd=1) 
rn2=c(rn2,rnorm(20,mean=12,sd=2)) # bimodal distribution 
mean(rn1) 
mean(rn2) 
athi$wilcoxR(rn1,rn2) # two numerical vectors 
wt=wilcox.test(rn1,rn2,exact=FALSE)  
athi$wilcoxR(wt,n=200) # a test object 
cat = as.factor(c(rep('A',100),rep('B',100))) 
athi$wilcoxR(x=c(rn1,rn2),y=cat) # num ~ cat 
} 
 
\seealso{ 
  \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD},  
  \link[athi:athi_cohensW]{athi$cohensW}, \link[athi:athi_eta_squared]{athi$eta_squared}, 
  \link[athi:athi_epsilon_squared]{athi$epsilon_squared} 
} 
