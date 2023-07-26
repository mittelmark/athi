\name{athi$corr_plot} 
\alias{athi$corr_plot} 
\alias{athi_corr_plot} 
\title{ visualize a correlation matrix } 
\description{ 
    This function is plotting the pairwise correlations for the given 
    symmetric correlation matrix. 
} 
\usage{ athi_corr_plot(mt,text.lower=TRUE, text.upper=FALSE, 
                     pch=19,p.mat=NULL,alpha=0.05, 
                    cex.sym=5,cex.r=1,cex.lab=1.4,...)} 
\arguments{ 
\item{mt}{matrix with pairwise correlations} 
\item{text.lower}{should in the lower diagonal the correlation coefficient be shown, default: TRUE} 
\item{text.upper}{should in the upper diagonal the correlation coefficient be shown, default: FALSE} 
\item{pch}{the plotting symbol for the correlations, default: 19} 
\item{p.mat}{matrix with p-values to strike out insignificant p-values, default: NULL (not used)} 
\item{alpha}{significance threshold for `p.mat`, default: 0.05} 
\item{cex.sym}{character expansion for the correlation symbols, default: 5} 
\item{cex.r}{character expansion for the r-values if _text.lower_ or _text.upper_ are set to TRUE, default: 1} 
\item{cex.lab}{character expansion for the variable text labels, default: 1.4} 
\item{\ldots}{other arguments delegated to the plot function} 
} 
\value{NULL} 
\examples{ 
data(swiss) 
sw=swiss 
colnames(sw)=abbreviate(colnames(swiss),6) 
options(warn=-1) # avoid spearman warnings 
cr=athi$corr(sw,method='spearman') 
athi$corr_plot(cr$estimate,cex.sym=8,text.lower=TRUE, 
   cex.r=1.5,p.mat=cr$p.value) 
options(warn=0) 
} 
\seealso{ 
   \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr} 
} 
 
