\name{athi$mds_plot} 
\alias{athi$mds_plot} 
\alias{athi_mds_plot} 
\title{ Plot a data matrix or frame using Multidimensional Scaling } 
\description{ 
    This is a convinience method to plot a data set using MDS. 
} 
\usage{ athi_mds_plot(x,method="euclidean",p=0.5,row.labels=TRUE,points=FALSE, 
        col.labels='black',cex.labels=1, pch=19, grid=TRUE,...) } 
\arguments{ 
  \item{x}{ 
    data frame or matrix  
  } 
  \item{method}{ 
    distance measure 'euclidean', 'manhattan' or any other method supported by the dist method 
    or 'correlation' for Pearson correlation or 'spe' or 'spearman' for Spearman correlation, 
    default: 'euclidean' 
  } 
  \item{p}{ 
    exponent if distance measure is minkowski, default: 0.5 
  } 
  \item{row.labels}{should be row labels computed, if FALSE or if row.names are not existing, plotting characters are displayed, default: TRUE} 
  \item{points}{should with row labels as well points be ploted, default: FALSE}  
  \item{col.labels}{color for text labels, default: 'black'} 
  \item{cex.labels}{size of labels, default: 1} 
  \item{pch}{default plotting character, default: 19} 
  \item{grid}{should a grid being show, default: TRUE} 
  \item{\ldots}{delegating all remaining arguments to plot, points and text calls} 
} 
\value{NULL} 
\examples{ 
  data(iris) 
  # single plots 
  par(mfrow=c(1,2)) 
  athi$mds_plot(iris[,1:4],method="manhattan") 
  athi$mds_plot(iris[,1:4],method="manhattan",row.labels=FALSE) 
  # multiplot 
  opar=par(mai=c(0.1,0.1,0.5,0.1)) 
  athi$mds_plot(iris[,1:4], 
     method=c("cor","euclidean","canberra","mink","max","man"), 
     p=0.2,row.labels=FALSE,pch=15, 
     col=as.numeric(as.factor(iris$Species))+1) 
  par(opar) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
