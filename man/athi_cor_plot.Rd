\name{athi$cor_plot} 
\alias{athi$cor_plot} 
\alias{athi_cor_plot} 
\title{ visualize a correlation with abline and main statistics } 
\description{ 
    This function is plotting the standard xy-plot for two numerical variables and adds on top the  
      the main statistics, like the r-value, the confidence interval and the significance level. 
} 
\usage{ athi_cor_plot(x,y,method="pearson",col='blue',main=NULL,pch=19,...) } 
\arguments{ 
   \item{x}{vector with numerical values, missing values are allowed} 
   \item{y}{vector with numerical values, missing values are allowed} 
   \item{method}{type of correlation to be determined, either 'pearson', 'spearman' or 'kendall', default: 'pearson'} 
   \item{col}{plotting character color, default: 'blue'} 
   \item{main}{plotting title, if not given the main statistics are shown, to suppress this give an empty string here, default: NULL} 
   \item{pch}{plotting character, default: 19} 
   \item{\ldots}{other arguments delegated to the plotting function} 
} 
\value{NULL} 
\examples{ 
data(swiss) 
athi$cor_plot(swiss$Fertility,swiss$Agriculture, 
    xlab="Fertility",ylab="Agriculture") 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr} 
} 
 
