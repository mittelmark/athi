\name{athi$dpairs} 
\alias{athi$dpairs} 
\alias{athi_dpairs} 
\title{Improved pairs plot considering the data types} 
\description{ 
  The function \code{athi_dpairs} provides an improved pairs plot which accounts 
  for the data type of the actual variables. It will plot in the  
  lower diagonal xy-plots, box-plots or assoc-plots depending on the  
  two data types. In the upper diagonal effect sizes and stars for the p-values 
  for the tests (anova, t.test, chisq.test or cor.test) will be shown. In the diagonal  
  the data distribution will be outlined. This plot is usually an useful visualization for 3-8 variables. 
} 
\usage{athi_dpairs(data, col.box='grey80', col.xy="grey60", cex.diag=2.5, order=TRUE,  
    pch=19)} 
\arguments{ 
  \item{data}{ 
    data frame with columns of class factor, numeric or integer. 
  } 
  \item{col.box}{ 
    colors for the boxplots, either a single value or a vector of colors for each level of a factor variable, default; 'grey80' 
  } 
  \item{col.xy}{ 
    colors for the xy-plots, either a single value of a vector which is as long as the number of data points, default: 'grey60' 
  } 
  \item{cex.diag}{ 
    character expansion for the diagonal texts 
  } 
  \item{order}{ 
    should the variables be ordered by data type and name, this is recommended as it orders the plots, starting with assocplots, then boxplots and finally xyplots, default: TRUE 
  } 
  \item{pch}{ 
    plotting character for xy-plots, default 19 (round circle). 
  } 
} 
\examples{ 
  data(iris) 
  ##options(repr.plot.width=12, repr.plot.height=12) 
  grDevices::dev.new(width = 10, height = 10) 
  par(omi = c(0.8, 0.4,0.4,0.4)) 
  athi$dpairs(iris,col.box=2:4,col.xy=rep(c(2:4),each=50), 
     cex.diag=1.6) 
  athi$dpairs.legend(levels(iris$Species),col=2:4) 
  library(MASS) 
  btwt=birthwt;  
  for (col in c('low','race','smoke','ptl','ht','ui','ftv')) {  
     btwt[,col]=as.factor(btwt[,col])  
  } 
  grDevices::dev.new(width = 16, height = 16) 
  par(omi=rep(0.2,4)) 
  athi$dpairs(btwt[,c(1:5,10)],cex.diag=1.6) 
      mtext('Birth-Weight data',side=3,outer=TRUE, 
      cex=1.5,line=1) 
} 
\seealso{ \code{\link[athi:athi-class]{athi-class}}, 
\code{\link[athi:athi$dpairs.legend]{athi$dpairs.legend}}} 
 
