\name{athi$pca_oncor} 
\alias{athi$pca_oncor} 
\alias{athi_pc_oncor} 
\title{Perform a PCA on a correlation matrix.} 
\description{ 
The function `athi$pca_oncor` does a PCA using eigenvector eigenvalue decomposition 
  on a correlation matrix. PCA usually performs Pearson correlation internally what 
  leads to a highly outlier sensitive analysis. If the user decides 
  to use a method like Spearman or even bi-seriell, polychoric or for nominal data 
  effect size measures like Cohen's W this method here can be used. Note that this 
  does not return new coordinates for the sample as the sample contribution is lost in the 
  correlation matrix. The method might however be used to check if the results between 
  Pearson and Spearman PCA are similar or does  outliers lead to a completly different result. 
} 
\usage{ athi_pca_oncor(x) } 
\arguments{ 
\item{x}{a symmetric matrix usually with pairwise correlations} 
} 
\value{PCA like list object with components sd and rotation} 
\examples{ 
data(USArrests) 
C=cor(USArrests) 
athi$pca_oncor(C) 
D=cor(USArrests,method="spearman")  
athi$$pca_oncor(D) 
} 
 
