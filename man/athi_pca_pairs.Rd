\name{athi$pca_pairs} 
\alias{athi$pca_pairs} 
\alias{athi_pca_pairs} 
\title{Improved pairs plot for pca objects.} 
\description{ 
  The function `athi$pca_pairs` provides an improved pairs plot for 
  visualizing the pairwise scores of the individual components of an analyses  
  using the function `prcomp`. In contrast to the default `pairs` function  
  this plot visualizes in the diagonal as well the variances and  
  a density line for the component scores. 
} 
\usage{ athi_pca_pairs(pca,n=10,groups=NULL, col='black',pch=19,legend=FALSE,...) } 
\arguments{ 
\item{pca}{pca object which was created using the function `prcomp`.} 
\item{n}{maximal number of components to visualize, default: 10} 
\item{groups}{vector with classes having the same length than the inout matrix for prcomp has rows, default: NULL} 
\item{col}{colors for the plotting, character, default: 'black'} 
\item{pch}{plotting, symbol, default: 19} 
\item{legend}{should the legend be displayed on top, default: FALSE} 
\item{\ldots}{additional arguments delegated to the standard `pairs` function} 
} 
\value{NULL} 
\examples{ 
data(iris) 
pci=prcomp(iris[,1:4],scale=TRUE) 
athi$pca_pairs(pci,pch=15,groups=iris[,5], 
   legend=TRUE,oma=c(5,4,4,4),col=as.numeric(iris[,5])+1) 
} 
 
