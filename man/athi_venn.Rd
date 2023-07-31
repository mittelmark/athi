\name{athi$venn} 
\alias{athi$venn} 
\alias{athi_venn} 
\title{Plot Venn diagram for logical relations between two or three sets.} 
\description{This function combines two data frames or matrices even if they have different 
  column names. In each data frame or matrix missing columns are first filled up 
  with NA's. Thereafter data is fused using rbind. Column order is determined based  
  on the first given data. 
} 
\usage{ athi_venn(x,y=NULL,z=NULL,vars=NULL,col=c("#cc888899","#8888cc99","#88cc8899"),cex=1.6,...) } 
\arguments{ 
\item{x}{data frame or matrix or vector, in the latter case y and for 3 sets as well z must be given} 
\item{y}{vector if _x_ is vector, default: NULL} 
\item{z}{vector if _x_ and _y_ are vectors, default: NULL} 
\item{vars}{variable names to display if x, y and z are vectors, default: NULL} 
\item{col}{background colors for the circles, default: c("#cc888899","#8888cc99","#88cc8899")} 
\item{cex}{character expansion for the text characters, default: 1.6} 
\item{\ldots}{argument delegated to the plot function} 
} 
\value{NULL} 
\examples{ 
X=matrix(rnorm(2700),ncol=3) 
colnames(X)=c("A","B","C") 
X=X>0.4 
Y=X[,1:2] 
Y=Y>0.7 
par(mfrow=c(2,2),mai=rep(0.1,4),pty='s') 
athi$venn(X) 
athi$venn(Y) 
athi$venn(x=LETTERS[1:9],y=LETTERS[3:6],z=LETTERS[4:12],vars=c('Xvar','Yvar','Zvar')) 
athi$venn(x=LETTERS[1:9],y=LETTERS[3:6],vars=c('Xvar','Yvar')) 
} 
\seealso{ 
  \link[athi:athi-class]{athi-class} 
} 
  
