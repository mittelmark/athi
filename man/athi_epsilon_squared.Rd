\name{athi$epsilon_squared} 
\alias{athi$epsilon_squared} 
\alias{athi_epsilon_squared} 
\title{ Effect sizes measure for Kruskal test } 
\description{ 
Calculate the effect size epsilon-squared for variables of a kruskal.test 
} 
\usage{ athi_epsilon_squared(x,y) } 
\arguments{ 
\item{x}{vector with numerical values} 
\item{y}{vector with factor values} 
} 
\details{ 
 The function `athi$epsilon_squared` calculates the effect size for a Kruskal test. 
 Cohen's rule of thumb for interpretation is: around 0.01 small, around 0.09 medium and around 0.25 or higher we have a large effect. 
 You can convert Epsilon-squared to a Pearson r coefficient by using the sqrt of The Epsilon-square value. 
 
Please note that these rules of thumb are not useful for highly dependent outcome  
variables (death for instance) these rules might not be useful and as well lower 
values might be of practical relevance. 
} 
\value{numerical value, effect size Epsilon-squared} 
\examples{ 
data(iris) 
athi$epsilon_squared(iris$Sepal.Length,iris$Species) 
# two factor example as well for wilcox.test possible 
data(ToothGrowth) 
athi$epsilon_squared(ToothGrowth$len,as.factor(ToothGrowth$dose)) 
# close to r-square of spearman! 
cor(ToothGrowth$len,ToothGrowth$dose,method="spearman")^2 
} 
 
\seealso{ 
   \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD},  
    \link[athi:athi_cohensW]{athi$cohensW}, \link[athi:athi_eta_squared]{athi$eta_squared}   
} 
 
