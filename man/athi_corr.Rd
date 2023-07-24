\name{athi$corr} 
\alias{athi$corr} 
\alias{athi_corr} 
\title{ Pairwise correlations for a matrix or data frame with estimate, p-values and confidence intervals } 
\description{ 
    Method to determine pairwise correlations and their statistical properties. 
} 
\usage{ athi_corr(x,method='pearson',use='pairwise.complete.obs') } 
\arguments{ 
   \item{x}{matrix or data frame where the variables are in the columns, NAs are allowed.} 
   \item{method}{type of correlation to be determined, either 'pearson', 'spearman' or 'kendall', default: 'pearson'} 
   \item{use}{how to deal with NA's, default: 'pairwise.complete.obs'} 
} 
\value{Returns list with the following components: 
\itemize{ 
   \item estimate - matrix with correlation values 
   \item p.value - matrix with p-values 
   \item lower - lower range of the 95 percent confidence interval 
   \item upper - upper range of the 95 percent confidence interval 
   \item method - character string with the used correlation method 
} 
} 
\details{ 
This function is an extension to the `cor` function as it as well calculates all p-values and the confidence intervals for the 
correlation coefficient. 
} 
\examples{ 
data(swiss) 
res=athi$corr(swiss) 
ls(res) 
lapply(res[1:2],round,2) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot}  
} 
