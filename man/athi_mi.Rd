\name{athi$mi} 
\alias{athi$mi} 
\alias{athi_mi} 
\title{ measure mutual information} 
\description{ 
    Return the mutual information for two vectors, a matrix or data frame,  or a binned table. 
} 
\usage{ athi_mi(x,y=NULL,breaks=4,norm=FALSE) } 
\arguments{ 
  \item{x}{either a binned table, a matrix or data.frame or a numerical vector} 
  \item{y}{numerical vector, required only if x is not a binned table or matrix or data.frame} 
  \item{breaks}{number of breaks to create a binned table if x and y are numerical vectors, default: 4} 
  \item{norm}{if input is given should the matrix be normalized by dividing the off-diagonal values by the mutual information in the diagonals, so the self mutual information, default: FALSE} 
} 
\value{mutual information value as scalar if input is table or two vectors or as matrix if input is matrix or data.frame} 
\examples{ 
rn1=rnorm(100,mean=10,sd=1); 
rn2=rn1+0.5*rnorm(100) 
cor(rn1,rn2) # high 
cor(rn1,sample(rn2)) #random  
athi$mi(rn1,rn2) # high  
athi$mi(rn1,sample(rn2)) #random 
athi$mi(rn1,rn2,breaks=4) 
athi$mi(rn1,rn2,breaks=7) 
data(swiss) 
round(athi$mi(swiss),2) 
round(athi$mi(swiss,norm=TRUE),2) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr} 
} 
 
