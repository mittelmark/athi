\name{athi$eta_squared} 
\alias{athi$eta_squared} 
\alias{athi_eta_squared} 
\title{ Effect sizes measure for ANOVA } 
\description{ 
Calculate the effect size for an ANOVA or a linear model with two variables. 
} 
\usage{ athi_eta_squared(x,y=NULL) } 
\arguments{ 
\item{x}{vector with numerical values or a linear model or an aov object} 
\item{y}{either a factor variable or NULL if x is given as model,default: NULL} 
} 
\details{ 
 The function `athi$eta_squared` (omega) calculates the effect size for an ANOVA. 
 Cohen's rule of thumb for interpretation is: around 0.01 small, around 0.09 medium and around 0.25 or higher we have a large effect. 
 You can convert Eta-squared to a Pearson r coefficient by using the sqrt of eta-square. 
 
Please note that these rules of thumb are not useful for highly dependent outcome  
variables (death for instance) these rules might not be useful and as well lower 
values might be of practical relevance. 
} 
\value{numerical value, effect size Eta-squared} 
\examples{ 
data(iris) 
etaSquared=athi$eta_squared 
etaSquared(iris$Sepal.Length,iris$Species) 
etaSquared(lm(iris$Sepal.Length ~ iris$Species)) 
etaSquared(aov(iris$Sepal.Length ~ iris$Species)) 
etaSquared(aov(Sepal.Length ~ Species+Sepal.Width+Petal.Length,data=iris)) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD}, \link[athi:athi_cohensW]{athi$cohensW}  
} 
 
