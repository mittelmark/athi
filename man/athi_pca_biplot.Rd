\name{athi$pca_biplot} 
\alias{athi$pca_biplot} 
\alias{athi_pca_biplot} 
\title{ Improved biplot for pca objects. } 
\description{ 
The function `athi$pca_biplot` provides an improved biplot for 
visualizing the pairwise scores of individual principal components of  
an object created using the function `prcomp`. In contrast to the default  
biplot function  this plot visualizes the data as points and not row numbers, 
it allows to display groups using color codes and distribution ellipses. 
} 
\usage{ athi_pca_biplot(pca,pcs=c("PC1","PC2"), 
                       pch=19,col='black', 
                       arrows=TRUE,arrow.fac=1, 
                       ellipse=FALSE,ell.fill=FALSE,xlab=NULL,ylab=NULL,...) } 
\arguments{ 
\item{pca}{pca object of class `prcomp`, created using the function `prcomp`} 
\item{pcs}{the components to plot, default: c('PC1','PC2')} 
\item{pch}{plotting character, default: 19} 
\item{col}{plotting color, default: black} 
\item{arrows}{should loading arrows be displayed, default: TRUE} 
\item{arrow.fac}{scaling factor for arrow length, default: 1} 
\item{ellipse}{should 85 and 95 confidence intervals for the chisq distribution be shown. If this is shown colors for each group using the col argument must be given, default: FALSE} 
\item{ell.fill}{should a filled 85 percent confidence interval be shown, colors will be used from the plotting color with opacity, default: FALSE} 
\item{xlab}{custom xlab, if not given the PC name with variance in percent is shown, default: NULL} 
\item{ylab}{custom ylab, if not given the PC name with variance in percent is shown, default: NULL} 
\item{\ldots}{additional arguments delegated to the standard plot function} 
} 
\value{NULL} 
\examples{ 
par(mai=c(0.8,0.8,0.2,0.6),mfrow=c(1,2)) 
data(iris) 
pci=prcomp(iris[,1:4],scale=TRUE) 
athi$pca_biplot(pci,col=rep(2:4,each=50),ellipse=TRUE,ell.fill=TRUE, 
    arrow.fac=2.3,arrows=TRUE,main="biplot") 
legend('topright',pch=19,col=2:4,levels(iris$Species)) 
# standard score plot 
athi$pca_biplot(pci,col=rep(2:4,each=50),ellipse=FALSE, 
   arrow.fac=2.3,arrows=FALSE,main="scoreplot") 
} 
 
