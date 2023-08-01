\name{athi$pca_plot} 
\alias{athi$pca_plot} 
\alias{athi_pca_plot} 
\title{Improved bar or screeplot for pca objects.} 
\description{ 
The function `athi$pca_plot` provides an improved bar- or screeplot for 
visualizing the variances of the individual components of an analyses  
using the function _prcomp_. In contrast to the default plot function  
this plot visualize cumulative and individual variances in percent. 
} 
\usage{ athi_pca_plot(pca,n=10,type="bar", cex=1.5,  
                     legend=TRUE,xlab="Components",ylab="Variance (%)", 
                     pc.col=c("light blue","grey"),...)} 
\arguments{ 
\item{pca}{pca object which was created using the function `prcomp`} 
\item{n}{maximal number of components to visualize, default: 10} 
\item{type}{plotting type either "bar" or "scree", default: "bar"} 
\item{cex}{character expansion for the legend and the screeplot plotting characters, default: 1.5} 
\item{legend}{should the legend be displayed on top, default: TRUE} 
\item{pc.col}{colors for the PC variances, first individual, second color for the cumulative variance, default: c("light blue","grey")} 
\item{\ldots}{additional arguments delegated to the standard plot function} 
} 
\value{NULL} 
\examples{ 
data(iris) 
par(mfrow=c(1,2)) 
pcai=prcomp(iris[,1:4],scale=TRUE) 
athi$pca_plot(pcai) 
athi$pca_plot(pcai,type="scree",legend=FALSE) 
} 
 
