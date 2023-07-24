\name{athi$box_plot} 
\alias{athi$box_plot} 
\alias{athi_box_plot} 
\title{ visualize a mean comparison with boxplot and main statistics } 
\description{ 
    This function is plotting the standard boxplot for a numerical data against a factor variabl and adds on top the  
    the main statistics, like the Cohen's d, or the Eta-squared value, the significance level and for t-tests the   
    confidence interval as well. 
} 
\usage{ athi_box_plot(x,y=NULL,data=NULL,col='grey80',grid=TRUE,main=NULL,...) } 
\arguments{ 
   \item{x}{vector with numerical values or a formula, missing values are allowed} 
   \item{y}{vector with categorical data (factor), required if x is not a formula, default: NULL} 
   \item{data}{data frame, required if x is a formula, default: NULL} 
   \item{col}{plotting character color, default: 'grey80'} 
   \item{grid}{should a grid being plotted, default: TRUE} 
   \item{main}{plotting title, if not given the main statistics are shown, to suppress this give an empty string here, default: NULL} 
   \item{\ldots}{other arguments delegated to the default boxplot plotting function} 
} 
\value{NULL} 
\examples{ 
data(iris) 
athi$box_plot(Sepal.Length ~ Species, data=iris,col=2:4) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot} 
} 
 
