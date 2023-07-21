\name{athi$lm_plot} 
\alias{athi$lm_plot} 
\alias{athi_lm_plot} 
\title{ Plot a linear model with confidence intervals } 
\description{ 
    This is a convinience method to plot a linear model for confidence intervals for 
    the slope and for the predictions based on the model. 
} 
\usage{ athi_lm_plot(x,y=NULL, data=NULL,col="blue",pch=19,col.lm="red",col.plm="red",col.pi="blue", 
                      grid=TRUE,polygon=TRUE,col.polygon="#cccccc33",xlab=NULL,ylab=NULL,...)} 
\arguments{ 
  \item{x}{ 
    vector of numerical values or a formula 
  } 
  \item{y}{ 
    vector of numerical values, ignored if x is a formula 
  } 
  \item{data}{ 
    optionsal data frame containing the variables for the formula 
  } 
  \item{col}{scalar or vector for the color for the plotting character default: 'blue'} 
  \item{pch}{plotting character, default: 19} 
  \item{col.lm}{color for the regression line, default: 'red'} 
  \item{col.plm}{color for the regression line confidence interval, default: 'red'} 
  \item{col.pi}{color for the prediction confidence interval, default: 'blue'} 
  \item{grid}{should a grid be drawn in the plot, default: TRUE} 
  \item{polygon}{should the confidence interval for the regression line been shown as transparent polygon, default: TRUE} 
  \item{col.polygon}{the color for the polygon, default: "#cccccc33"} 
  \item{xlab}{the x-label, if not given chosen by the variable name, default: NULL} 
  \item{ylab}{the y-label, if not given chosen by the variable name, default: NULL} 
  \item{\ldots}{other arguments which will be forwarded to the plot function} 
 } 
\value{NULL} 
\examples{ 
par(mfrow=c(1,2)) 
data(iris)  
athi$lm_plot(iris$Sepal.Width, iris$Sepal.Length, 
   col=as.numeric(iris$Species)+1,col.pi="bisque4", 
   col.lm="black",xlab="Sepal.Width",ylab="Sepal.Length") 
props=c(0.0441,0.0133,0.0183,0.0238,0.0389, 
        0.0648,0.0275,0.0704,0.0796,0.036,0.0132, 
        0.108,0.136,0.0383,0.1008) 
years=2005:2019 
athi$lm_plot(years,props,ylim=c(0,0.3),xlab="Year",ylab="Proportion", 
   col.pi=NULL,col.plm='#FFB0B066',col.polygon='#FFB0B066') 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}  
} 
 
