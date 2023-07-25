\name{athi$qr_plot} 
\alias{athi$qr_plot} 
\alias{athi_qr_plot} 
\title{ Plot quantile regression models } 
\description{ 
    This is a convinience method to plot quantile regression models and giving 
    optional percentile intervals for a given range of predictions. 
    The slope and the intercept for the different quantiles are returned as well if requested. 
} 
\usage{ athi_qr_plot(x,data,quantiles=c(0.05,0.1,0.5,0.9,0.95),pred=NULL,plot=TRUE,...)}  
\arguments{ 
  \item{x}{ 
    formula for two numerical variables 
  } 
  \item{data}{ 
    data frame containing the variables for the formula 
  } 
  \item{quantiles}{ 
    the requested quantiles, default: c(0.05,0.1,0.5,0.9,0.95) 
  } 
  \item{pred}{ 
    vector for prediction values, default: NULL 
  } 
  \item{plot}{ 
    should the data being plotted, default: TRUE 
  } 
  \item{\ldots}{other arguments which will be forwarded to the plot function} 
} 
\value{returns list with intercept and coefficients if requested (invisible)} 
\examples{ 
data(iris)  
athi$qr_plot(Sepal.Width ~ Sepal.Length,data=iris[51:151,]) 
res=athi$qr_plot(Sepal.Width ~ Sepal.Length,data=iris[51:151,],pred=c(5,5.5,6,6.5,7,7.5,8)) 
res$centiles 
res$coef 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_lm_plot]{athi$lm_plot} 
} 
 
