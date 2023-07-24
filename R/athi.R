#' \docType{data}
#' \name{athi-class}
#' \alias{athi-class}
#' \alias{athi}
#' \title{athi - functions for the Guelpe Summerschool}
#' \description{This environment has some useful function required for the
#' the Guelpe Summer school Human Biology and Publich Health - Data Analysis and Statistics.
#' }
#' \format{Object of class environment with some functions for statistics}
#' \details{
#' \describe{
#'   \item{\link[athi:athi_box_plot]{athi$box_plot(x,y=NULL)}}{Extended version of the boxplot with main statistic values on top.}
#'   \item{\link[athi:athi_cdist]{athi$cdist(x,method="spearman",type="abs")}}{Calculate correlation distances.}
#'   \item{\link[athi:athi_cohensD]{athi$cohensD(x,g,paired=FALSE)}}{Calculate effect size for difference between two means.}
#'   \item{\link[athi:athi_cohensW]{athi$cohensW(x,p=NULL)}}{Calculate effect size for categorical data.}
#'   \item{\link[athi:athi_corr]{athi$corr(x,method="pearson",use="pairwise.complete.obs")}}{Calculate pairwise correlations and the statistics.}
#'   \item{\link[athi:athi_cor_plot]{athi$cor_plot(x,y,method="pearson",...)}}{Extended version of the xy-plot with main statistics in the title.}
#'   \item{\link[athi:athi_df2md]{athi_df2md(x,caption='',rownames=TRUE)}}{Print a matrix or data frame as a Markdown table}
#'   \item{\link[athi:athi_eta_squared]{athi$eta_squared(x,y=NULL)}}{Calculate effect size for ANOVA.}
#'   \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values.}
#'   \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values.}
#'   \item{\link[athi:athi_lm_plot]{athi$lm_plot(x,y=NULL,data=NULL,...)}}{plot a linear model with confidence intervals.}
#'   \item{\link[athi:athi_mds_plot]{athi$mds_plot(x,method="euclidean",...)}}{plot a multidimensional scaling.}
#'   \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data.}
#'   \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame.}
#'   \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type.}
#'   \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type.}
#'   \item{\link[athi:athi_report_pvalue]{athi$report_pvalue(p,star=FALSE)}}{report a p-value using the significance thresholds.}
#' }
#' All methods are given in to forms: first they are collected in an environment `athi` which allow you to save this object for instance as
#' a RDS file and then give it away with your analysis to allow other users to redo or extend the analysis without having to install the library. 
#' Secondly the methods have the athi prefix followed by an underline which is compatbible with the default R documentation system.
#' }
#' \examples{
#' # list all methods
#' ls(athi)
#' athi$ref_score(100,age=4,sex="M",type="height")
#' head(athi$ref_table(sex="F",type="height"))
#' }
#' \author{Detlef Groth <email: dgroth@uni-potsdam.de>}

athi=new.env()

#' \name{athi$box_plot}
#' \alias{athi$box_plot}
#' \alias{athi_box_plot}
#' \title{ visualize a mean comparison with boxplot and main statistics }
#' \description{
#'     This function is plotting the standard boxplot for a numerical data against a factor variabl and adds on top the 
#'     the main statistics, like the Cohen's d, or the Eta-squared value, the significance level and for t-tests the  
#'     confidence interval as well.
#' }
#' \usage{ athi_box_plot(x,y=NULL,data=NULL,col='grey80',grid=TRUE,main=NULL,...) }
#' \arguments{
#'    \item{x}{vector with numerical values or a formula, missing values are allowed}
#'    \item{y}{vector with categorical data (factor), required if x is not a formula, default: NULL}
#'    \item{data}{data frame, required if x is a formula, default: NULL}
#'    \item{col}{plotting character color, default: 'grey80'}
#'    \item{grid}{should a grid being plotted, default: TRUE}
#'    \item{main}{plotting title, if not given the main statistics are shown, to suppress this give an empty string here, default: NULL}
#'    \item{\ldots}{other arguments delegated to the default boxplot plotting function}
#' }
#' \value{NULL}
#' \examples{
#' data(iris)
#' athi$box_plot(Sepal.Length ~ Species, data=iris,col=2:4)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot}
#' }
#' 
athi$box_plot = function (x,y=NULL,data=NULL,col='grey80',grid=TRUE,main=NULL,...) {
    if (class(x)[1]=="formula") {
        if (!is.data.frame(data)) {
            df <- model.frame(x)
            colnames(df)=gsub(".+\\$","",colnames(df))
        } else {
            df <- model.frame(x, data = data)
        }
        df=df[,c(1,2)]
        cnames=colnames(df)
        x=df[,1]
        y=df[,2]
    } else {
        cnames = c("x","y")
    }
    if ("xlab" %in% names(list(...))) {
        cnames[2]=list(...)[["xlab"]]
    }
    if ("ylab" %in% names(list(...))) {
        cnames[1]=list(...)[["ylab"]]
    }
    boxplot(x ~ y,col=col,xlab=cnames[2],ylab=cnames[1])
    if (grid) {
        grid()
        boxplot(x ~ y,col=col,add=TRUE,...)
    }
    if (length(levels(y)) == 2) {
        t=t.test(x ~ y)
        d=athi$cohensD(x,y)
        
        r=paste("d = ",round(d,2),athi_report_pvalue(t$p.value,star=TRUE)," CI95%[",round(t$conf.int[1],3),",",round(t$conf.int[2],3),"]",sep="")
        mtext(r,side=3,adj=1)
    } else {
        raov=aov(x ~ y)
        e=athi$eta_squared(x,y)
        saov=summary(raov)
        r=paste("Eta.sq = ",round(e,2),athi_report_pvalue(saov[[1]][1,5],star=TRUE),sep="")
        mtext(r,side=3,adj=1)
        
    }
    box()
}
#' \name{athi$cdist}
#' \alias{athi$cdist}
#' \alias{athi_cdist}
#' \title{ correlation distances }
#' \description{
#'     Calculate correlation distance.
#' }
#' \usage{ athi_cdist(x,method="pearson",type="abs") }
#' \arguments{
#'   \item{x}{
#'     data frame or matrix 
#'   }
#'   \item{method}{
#'     correlation measure, either 'pearson', 'spearman','kendall', default: 'pearson'
#'   }
#'   \item{type}{
#'     either absolute, squared or negative (negativly correlated items are far away, default: 'absolute'
#'   }
#' }
#' \value{Object of class dist.}
#' \examples{
#'     athi$cdist(t(iris[,1:4]))
#'     athi$cdist(t(iris[,1:4]),method="spearman",type="square")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'


athi$cdist <- function (x,method="pearson",type="abs") {
    D=cor(t(x),method=method)
    if (type=="abs") {
        D=stats::as.dist(1-abs(D))
    } else if (type == "square") {
        D=stats::as.dist(1-D^2)
    } else if (type == "negative") {
        D = stats::as.dist(1-((D+1)/2))
    } else {
        stop("Error: Unknown cdist type, valid types are 'abs',  'square' or 'negative'!")
    }
    return(D)
}
#' \name{athi$cohensD}
#' \alias{athi$cohensD}
#' \alias{athi_cohensD}
#' \title{ Effect sizes measure for difference between two means }
#' \description{
#'     Calculate the effect size for the difference between two means, divided by the pooled standard deviation.
#' }
#' \usage{ athi_cohensD(x,g,paired=FALSE) }
#' \arguments{
#'  \item{x}{vector with numerical values}
#'  \item{g}{vector with two grouping variables, having the same length as num}
#'  \item{paired}{are the data paired, default: FALSE}
#' }
#' \details{
#'  The function cohensD calculates the effect size for the difference between two means.
#'   Due to Cohen's rule of thumb values of around 0.2 are considered to stand 
#'   for small effects, values of around 0.5 represent medium effects and values of around 0.8 
#'   and larger represent large effects. 
#' 
#' Please note that these rules of thumb are not useful for highly dependent 
#'  outcome variables (death for instance) these rules might not be useful and as 
#'  well lower values might be of practical relevance.
#' }
#' \value{numerical value}
#' \examples{
#' cohensD=athi$cohensD
#' set.seed(125)
#' data(sleep)
#' with(sleep,cohensD(extra,group))
#' x1=rnorm(100,mean=20,sd=1)
#' x2=rnorm(100,mean=22,sd=1)
#' g1=rep('A',100)
#' g2=rep('B',100)
#' # difference should be around 2SD
#' cohensD(c(x1,x2),as.factor(c(g1,g2)))
#' # biseriell correlation coefficient as alternative
#' # value is as well large
#' cor(c(x1,x2),as.numeric(as.factor(c(g1,g2))))
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'


athi$cohensD <- function (x, g,paired=FALSE) {
    num=x
    cat=g
    if (paired) {
        tt=t.test(num ~ cat,paired=paired)
        return(tt$statistic[[1]]/sqrt(length(num/2)))
    }   
    tt.agg=aggregate(num,by=list(cat),
        mean,na.rm=TRUE)
    pooledSD <- function(x, y) {
        x=x[!is.na(x)]
        y=y[!is.na(y)]
        sq.devs <- (c(x - mean(x), y - mean(y)))^2
         n <- length(sq.devs)
        return(sqrt(sum(sq.devs)/(n - 2)))
    }
    d=abs(tt.agg$x[1]-tt.agg$x[2])/pooledSD(
        num[cat==levels(cat)[1]],
        num[cat==levels(cat)[2]])
    return(d)
}

#' \name{athi$cohensW}
#' \alias{athi$cohensW}
#' \alias{athi_cohensW}
#' \title{ Effect sizes measure for categorical data }
#' \description{
#'     Calculate the effect size for 2x2 and larger contingency tables as well as for single variables.
#' }
#' \usage{ athi_cohensW(x,p=NULL) }
#' \arguments{
#'  \item{x}{contingency table or vector of count data.}
#'  \item{p}{expected proportions, required if x is a vector of count data, in case 'x' has length of two a single value, 
#'    can be given otherwise as many values as the length of x, default: NULL}
#' }
#' \details{
#'  The function `athi$cohensW` (omega) calculates the effect size for contingency tables. 
#'   Due to Cohen's rule of thumb values of around 0.1 are considered to stand 
#'   for small effects, values of around 0.3 represent medium effects and values 
#'   above 0.5 or higher represent large effects. 
#' 
#'   Please note that these rules of thumb are not useful for highly dependent outcome 
#'   variables (death for instance) these rules might not be useful and as well lower
#'   values might be of practical relevance.
#' }
#' \value{numerical value, effect size Cohen's W (omega)}
#' \examples{
#' data(Titanic)
#' Titanic[1,1,,]
#' athi$cohensW(Titanic[1,1,,])
#' # Data from New Eng. J. Med. 329:297-303, 1993
#' azt=as.table(matrix(c(76,399,129,332), byrow=TRUE,ncol=2))
#' rownames(azt)=c("AZT","Placebo")
#' colnames(azt)=c("DiseaseProgress", "NoDiseaseProgress")
#' athi$cohensW(azt)
#' # number of boys (25) and girls (15) in a hospital which deviates 
#' # from 50/50 or 51/49 ratios
#' prop.table(c(25,15))
#' (prop.table(c(25,15))-0.5)*2
#' athi$cohensW(c(25,15),p=0.5)
#' athi$cohensW(c(25,15),p=c(0.51,0.49))
#' # most extrem case 40 boys and 0 girls
#' athi$cohensW(c(40,0),p=0.5)
#' athi$cohensW(c(40,0),p=c(0.51,0.49)) # max value here around 2*0.49
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD} 
#' }
#'


athi$cohensW <- function (x,p=NULL) {
    if (is.table(x) | is.matrix(x)) {
        tab=x
        pe=prop.table(chisq.test(tab)$expected)
        po=prop.table(tab)
        w=sqrt(sum(((po-pe)^2)/pe))
        return(w[[1]])
    } else if (is.null(p)) {
        stop('Error: If x is a vector, p must be given!')
    } else {
        if (length(x) == 2 & length(p) == 1) {
            p=c(p,1-p)
            po=prop.table(x)
            pe=p
        } else if  (length(x) == length(p)) {
            po=prop.table(x)
            pe=p
        } else {
            stop('Error: for more than 2 categories the
                 given proportion vector p must have the
                 same length as the given count vector x')
        }
        w = sqrt(sum(((po-pe)^2)/pe))
        return(w)
    }
}

#' \name{athi$corr}
#' \alias{athi$corr}
#' \alias{athi_corr}
#' \title{ Pairwise correlations for a matrix or data frame with estimate, p-values and confidence intervals }
#' \description{
#'     Method to determine pairwise correlations and their statistical properties.
#' }
#' \usage{ athi_corr(x,method='pearson',use='pairwise.complete.obs') }
#' \arguments{
#'    \item{x}{matrix or data frame where the variables are in the columns, NAs are allowed.}
#'    \item{method}{type of correlation to be determined, either 'pearson', 'spearman' or 'kendall', default: 'pearson'}
#'    \item{use}{how to deal with NA's, default: 'pairwise.complete.obs'}
#' }
#' \value{Returns list with the following components:
#' \itemize{
#'    \item estimate - matrix with correlation values
#'    \item p.value - matrix with p-values
#'    \item lower - lower range of the 95 percent confidence interval
#'    \item upper - upper range of the 95 percent confidence interval
#'    \item method - character string with the used correlation method
#' }
#' }
#' \details{
#' This function is an extension to the `cor` function as it as well calculates all p-values and the confidence intervals for the
#' correlation coefficient.
#' }
#' \examples{
#' data(swiss)
#' res=athi$corr(swiss)
#' ls(res)
#' lapply(res[1:2],round,2)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot} 
#' }

athi$corr <- function (x,method='pearson',use='pairwise.complete.obs') {
    mt=matrix(0,nrow=ncol(x),ncol=ncol(x))
    colnames(mt)=rownames(mt)=colnames(x)
    mt.pval=mt
    mt.lower=mt
    mt.upper=mt    
    diag(mt)=1
    for (i in 1:(ncol(x)-1)) {
        for (j in i:ncol(x)) {
            rt=cor.test(x[,i],x[,j],
                        method=method,use=use)
            mt[i,j]=mt[j,i]=rt$estimate
            mt.pval[i,j]=mt.pval[j,i]=rt$p.value
            mt.lower[i,j]=mt.lower[j,i]=rt$conf.int[1]
            mt.upper[i,j]=mt.upper[j,i]=rt$conf.int[2]
        }
    }
    return(list(estimate=mt,p.value=mt.pval,lower=mt.lower,upper=mt.upper,method=method))
}


#' \name{athi$cor_plot}
#' \alias{athi$cor_plot}
#' \alias{athi_cor_plot}
#' \title{ visualize a correlation with abline and main statistics }
#' \description{
#'     This function is plotting the standard xy-plot for two numerical variables and adds on top the 
#'       the main statistics, like the r-value, the confidence interval and the significance level.
#' }
#' \usage{ athi_cor_plot(x,y,method="pearson",col='blue',grid=TRUE,main=NULL,pch=19,...) }
#' \arguments{
#'    \item{x}{vector with numerical values, missing values are allowed}
#'    \item{y}{vector with numerical values, missing values are allowed}
#'    \item{method}{type of correlation to be determined, either 'pearson', 'spearman' or 'kendall', default: 'pearson'}
#'    \item{col}{plotting character color, default: 'blue'}
#'    \item{grid}{should a grid being plotted, default: TRUE}
#'    \item{main}{plotting title, if not given the main statistics are shown, to suppress this give an empty string here, default: NULL}
#'    \item{pch}{plotting character, default: 19}
#'    \item{\ldots}{other arguments delegated to the plotting function}
#' }
#' \value{NULL}
#' \examples{
#' data(swiss)
#' athi$cor_plot(swiss$Fertility,swiss$Agriculture,
#'     xlab="Fertility",ylab="Agriculture")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr}
#' }
#' 
athi$cor_plot = function (x,y,method="pearson",col='blue',grid=TRUE,main=NULL,pch=19,...) {
    p=cor.test(x,y,method=method,use="complete.obs")
    star=athi$report_pvalue(p$p.value,star=TRUE)
    r=paste('r = ',round(cor(x,y,method=method,
                             use="complete.obs"),2),star,sep="")
    r=paste(r," CI95%[",round(p$conf.int[1],2),",",round(p$conf.int[2],2),"]",sep="")
    plot(x~y,main=r,col=col,pch=pch,...);
    if (grid) {
        grid()
        points(x~y,col=col,pch=pch,...)
    }
    abline(lm(x~y),col=col,lwd=2)
    box()
}

#' \name{athi$eta_squared}
#' \alias{athi$eta_squared}
#' \alias{athi_eta_squared}
#' \title{ Effect sizes measure for ANOVA }
#' \description{
#' Calculate the effect size for an ANOVA or a linear model with two variables.
#' }
#' \usage{ athi_eta_squared(x,y=NULL) }
#' \arguments{
#' \item{x}{vector with numerical values or a linear model or an aov object}
#' \item{y}{either a factor variable or NULL if x is given as model,default: NULL}
#' }
#' \details{
#'  The function `athi$eta_squared` (omega) calculates the effect size for an ANOVA.
#'  Cohen's rule of thumb for interpretation is: around 0.01 small, around 0.09 medium and around 0.25 or higher we have a large effect.
#'  You can convert Eta-squared to a Pearson r coefficient by using the sqrt of eta-square.
#' 
#' Please note that these rules of thumb are not useful for highly dependent outcome 
#' variables (death for instance) these rules might not be useful and as well lower
#' values might be of practical relevance.
#' }
#' \value{numerical value, effect size Eta-squared}
#' \examples{
#' data(iris)
#' etaSquared=athi$eta_squared
#' etaSquared(iris$Sepal.Length,iris$Species)
#' etaSquared(lm(iris$Sepal.Length ~ iris$Species))
#' etaSquared(aov(iris$Sepal.Length ~ iris$Species))
#' etaSquared(aov(Sepal.Length ~ Species+Sepal.Width+Petal.Length,data=iris))
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD}, \link[athi:athi_cohensW]{athi$cohensW} 
#' }
#'

athi$eta_squared <- function (x,y=NULL) {
    if (class(x)[1] == "lm") {
        mod=x
        if (length(attr(mod$terms,"dataClasses"))==2) {
            # single factor given
            return(summary(mod)$r.squared)
        } else {
            class(x)="aov"
            return(athi$eta_squared(x))
        }
    } else if (class(x)[1] == "aov") {
        mod=x
        ss=sum(summary(mod)[[1]][,2])
        sq=summary(mod)[[1]][,2]/ss
        names(sq)=rownames(summary(mod)[[1]])
        sq=sq[1:(length(sq)-1)]
        return(sq)
    } else if (class(x)[1] == "numeric" & class(y)[1] == "factor") {
        mod=aov(x~y)
        return(as.vector((athi$eta_squared(mod))))

    } else {
        stop("Error: wrong call of 'eta_squared'! Call either 'athi$eta_squared(num,factor)' or with 'athi$eta_squared(lm(num~factor))'!")
    }
}

#' \name{athi$mds_plot}
#' \alias{athi$mds_plot}
#' \alias{athi_mds_plot}
#' \title{ Plot a data matrix or frame using Multidimensional Scaling }
#' \description{
#'     This is a convinience method to plot a data set using MDS.
#' }
#' \usage{ athi_mds_plot(x,method="euclidean",p=0.5,row.labels=TRUE,grid=TRUE,...) }
#' \arguments{
#'   \item{x}{
#'     data frame or matrix 
#'   }
#'   \item{method}{
#'     distance measure 'euclidean', 'manhattan' or any other method supported by the dist method or 'correlation', default: 'euclidean'
#'   }
#'   \item{p}{
#'     exponent if distance measure is minkowski, default: 0.5
#'   }
#'   \item{row.labels}{should be row labels computed, if FALSE or if row.names are not existing, plotting characters are displayed, default: TRUE}
#'   \item{grid}{should a grid being show, default: TRUE}
#'   \item{\ldots}{delegating all remaining arguments to plot, points and text calls}
#' }
#' \value{NULL}
#' \examples{
#'   data(iris)
#'   # single plots
#'   par(mfrow=c(1,2))
#'   athi$mds_plot(iris[,1:4],method="manhattan")
#'   athi$mds_plot(iris[,1:4],method="manhattan",row.labels=FALSE)
#'   # multiplot
#'   opar=par(mai=c(0.1,0.1,0.5,0.1))
#'   athi$mds_plot(iris[,1:4],
#'      method=c("cor","euclidean","canberra","mink","max","man"),
#'      p=0.2,row.labels=FALSE,pch=15,
#'      col=as.numeric(as.factor(iris$Species))+1)
#'   par(opar)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'
athi$mds_plot = function (x,method="euclidean",p=0.5,row.labels=TRUE,grid=TRUE,...) {
    if (length(method)>1) {
        opar=par(mfrow=c(2,ceiling(length(method)/2)))
    }
    for (m in method) {
        if (m %in% c("cor","correlation")) {
            # negative cor: low sim, -> high -> dissim
            d.obj=(1-stats::cor(t(x),use="pairwise.complete.obs")+1)/2
        } else {
            d.obj=dist(x, method = m,p=p)
        }
        cmd=stats::cmdscale(d.obj)
        limits = range(cmd)
        diff=diff(limits)*0.05
        xlim=c(limits[1]-diff,limits[2]+diff)
        ylim=xlim
        plot(cmd,type="n",xlim=xlim,ylim=ylim,xlab="Dim 1",ylab="Dim 2", ...)
        if (grid) {
            grid()
        }
        if (row.labels & length(rownames(x))== nrow(x)) {
            text(cmd,labels=rownames(x),...) 
        } else {
            points(cmd, ...)
        }
        if (length(method)>1) {
            title(m)
        }
    }
    if (length(method)>1) {
        par(opar)
    }
}

#' \name{athi$df2md}
#' \alias{athi$df2md}
#' \alias{athi_df2md}
#' \title{ Convert a data frame or a matrix into a Markdown table.}
#' \description{
#'   This function can be used within Rmarkdown documents to display easily
#'   a simple Markdown table. For more advanced use cases you should other commands
#'   such as the kable method from the knitr package.
#' }
#' \usage{ athi_df2md(x,caption='',rownames=TRUE) }
#' \arguments{
#'    \item{x}{matrix or data frame}
#'    \item{caption}{the caption for the table, it is just displayed below of the table, default: ''}
#'    \item{rownames}{should  the rownames be displayed, default: TRUE}
#' }
#' \value{prints to stdout}
#' \examples{
#' data(swiss)
#' athi$df2md(head(swiss))
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'


athi$df2md <- function(x,caption='',rownames=TRUE) {
    df=x
    cn <- colnames(df)
    if (is.null(cn[1])) {
        cn=as.character(1:ncol(df))
    }
    rn <- rownames(df)
    if (is.null(rn[1])) {
        rn=as.character(1:nrow(df))
    }
    if (rownames) {
        headr <- paste0(c("","", cn),  sep = "|", collapse='')
        sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), 
                                         collapse=''),length(cn)+1)), collapse ='')
    } else {
        headr <- paste0(c("", cn),  sep = "|", collapse='')
        sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), 
                                         collapse=''),length(cn))), collapse ='')
        
    }
    st <- "|"
    for (i in 1:nrow(df)){
        if (rownames) {
            st <- paste0(st, "**",as.character(rn[i]), "**|", collapse='')
        }
        for(j in 1:ncol(df)){
            if (j%%ncol(df) == 0) {
                st <- paste0(st, as.character(df[i,j]), "|", 
                             "\n", "" , "|", collapse = '')
            } else {
                st <- paste0(st, as.character(df[i,j]), "|", 
                             collapse = '')
            }
        }
    }
    fin <- paste0(c(headr, sepr, substr(st,1,nchar(st)-1)), collapse="\n")
    if (caption!='') {
        fin=paste0(fin,'\n',caption,'\n')
    }
    cat(fin)
}

#' \name{athi$lm_plot}
#' \alias{athi$lm_plot}
#' \alias{athi_lm_plot}
#' \title{ Plot a linear model with confidence intervals }
#' \description{
#'     This is a convinience method to plot a linear model for confidence intervals for
#'     the slope and for the predictions based on the model.
#' }
#' \usage{ athi_lm_plot(x,y=NULL, data=NULL,col="blue",pch=19,col.lm="red",col.plm="red",col.pi="blue",
#'                       grid=TRUE,polygon=TRUE,col.polygon="#cccccc33",xlab=NULL,ylab=NULL,...)}
#' \arguments{
#'   \item{x}{
#'     vector of numerical values or a formula
#'   }
#'   \item{y}{
#'     vector of numerical values, ignored if x is a formula
#'   }
#'   \item{data}{
#'     optionsal data frame containing the variables for the formula
#'   }
#'   \item{col}{scalar or vector for the color for the plotting character default: 'blue'}
#'   \item{pch}{plotting character, default: 19}
#'   \item{col.lm}{color for the regression line, default: 'red'}
#'   \item{col.plm}{color for the regression line confidence interval, default: 'red'}
#'   \item{col.pi}{color for the prediction confidence interval, default: 'blue'}
#'   \item{grid}{should a grid be drawn in the plot, default: TRUE}
#'   \item{polygon}{should the confidence interval for the regression line been shown as transparent polygon, default: TRUE}
#'   \item{col.polygon}{the color for the polygon, default: "#cccccc33"}
#'   \item{xlab}{the x-label, if not given chosen by the variable name, default: NULL}
#'   \item{ylab}{the y-label, if not given chosen by the variable name, default: NULL}
#'   \item{\ldots}{other arguments which will be forwarded to the plot function}
#'  }
#' \value{NULL}
#' \examples{
#' par(mfrow=c(1,2))
#' data(iris) 
#' athi$lm_plot(iris$Sepal.Width, iris$Sepal.Length,
#'    col=as.numeric(iris$Species)+1,col.pi="bisque4",
#'    col.lm="black",xlab="Sepal.Width",ylab="Sepal.Length")
#' props=c(0.0441,0.0133,0.0183,0.0238,0.0389,
#'         0.0648,0.0275,0.0704,0.0796,0.036,0.0132,
#'         0.108,0.136,0.0383,0.1008)
#' years=2005:2019
#' athi$lm_plot(years,props,ylim=c(0,0.3),xlab="Year",ylab="Proportion",
#'    col.pi=NULL,col.plm='#FFB0B066',col.polygon='#FFB0B066')
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'

athi$lm_plot = function (x,y=NULL, data=NULL,col="blue",pch=19,col.lm="red",col.plm="red",col.pi="blue",
                       grid=TRUE,polygon=TRUE,col.polygon="#cccccc33",xlab=NULL,ylab=NULL,...) {
    cnames=c('x','y')
    if (class(x)[1]=="formula") {
        if (!is.data.frame(data)) {
            df <- model.frame(x)
            colnames(df)=gsub(".+\\$","",colnames(df))
        } else {
            df <- model.frame(x, data = data)
        }
        df=df[,c(2,1)]
        cnames=colnames(df)
        colnames(df)=c('x','y')
        x=df$x
        y=df$y
    } else {
        df <- data.frame(x=x,y=y)
    }
    if (is.null(xlab)) {
        xlab=cnames[1]
    }
    if (is.null(ylab)) {
        ylab=cnames[2]
    }
    plot(y ~ x, data=df, pch=pch, col=col,xlab=xlab,ylab=ylab,...)
    if (grid) {
        grid (NULL,NULL, lty = 3, col = "grey30")
    }
    mod <- lm(y ~ x, data=df)
    new.x.df = data.frame(
        x = seq(from   = range(df$x)[1],
                to     = range(df$x)[2],
                length = 11 ))
    lty=list(upr=2,lwr=2,fit=1)
    lcol=list(upr=col.plm,lwr=col.plm,fit=col.lm)
    poly.y=c()
    poly.x=c()
    for (lim in c("upr","lwr","fit")) {
        nx=new.x.df$x
        ny=predict(mod, new.x.df,   
                            interval = "confidence" )[,lim]
        lines(x   = nx,
              y   = ny,
              col = lcol[[lim]],lty=lty[[lim]],lwd=2 )
        if (lim == "upr") {              
            poly.x=nx
            poly.y=ny
        } else if (lim == "lwr") {
            poly.x=c(poly.x,rev(nx))
            poly.y=c(poly.y,rev(ny))               
        }
    }
    if (polygon) {
        polygon(poly.x,poly.y,col=col.polygon,border=col.polygon)
        options(warn=-1)
        do.call('points',list(y ~ x, data=df, pch=pch, col=col,...))
        options(warn=0)
    }
    for (lim in c("upr","lwr")) {
        if (!is.null(col.pi)) {
            lines(
                  x   = new.x.df$x,
                  y   = predict( mod, new.x.df, 
                                interval = "prediction" )[ , lim ],
                  col = col.pi ,lty=2,lwd=2)
        }
     }
}

#' \name{athi$ref_table}
#' \alias{athi$ref_table}
#' \alias{athi_ref_table}
#' \title{ reference tables }
#' \description{
#'     Function to retrieve reference tables.
#' }
#' \usage{ athi_ref_table(sex,type) }
#' \arguments{
#'   \item{sex}{
#'     character string, either "M" for male/boy of "F" for female 
#'   }
#'   \item{type}{
#'     character string, either "height", "weight", "bmi", "muac" or "head"
#'   }
#' }
#' \details{
#'     Some more details ...
#' }
#' \value{data frame with the reference value}
#' \examples{
#'     athi$ref_table(sex="M",type="height")
#'     head(athi$ref_table(sex="F",type="height"))
#' }
#' 

athi$ref_table <- function (sex,type) {
    if (sex %in% c("M","male","Male","boy")) {
        sex = "boys"
    } else if (sex %in% c("F","female","Female","girl")) {
        sex =  "girls"
    }
    tab=utils::read.table(base::file.path(base::system.file(package="athi"),"references",
                                          paste(sex,"-",type,".tab",sep="")),sep="\t",header=TRUE)
    return(tab)
}

#' \name{athi$ref_score}
#' \alias{athi$ref_score}
#' \alias{athi_ref_score}
#' \title{ age corrected z-scores based on WHO references }
#' \description{
#'     Function to retrieve age corrected z-scores based on WHO references
#' }
#' \usage{ athi_ref_score(x,age,sex,type) }
#' \arguments{
#'   \item{x}{
#'     the numerical value for height (cm) or weight (kg) or ...
#'   }
#'   \item{age}{
#'     age in years, fractional numbers are possible 
#'   }
#'   \item{sex}{
#'     character string, either "M" for male/boy of "F" for female 
#'   }
#'   \item{type}{
#'     character string, either "height" or "weight"
#'   }
#' }
#' \details{
#'     Some more details ...
#' }
#' \value{data frame with the reference value}
#' \examples{
#'     athi$ref_score(100,age=4,sex="M",type="height")
#'     athi$ref_score(100,age=4,sex="F",type="height")
#'     head(athi$ref_table(sex="F",type="height"))
#' }
#' 

athi$ref_score <- function (x,age,sex,type) {
    this=athi
    type=tolower(type)
    sex=tolower(sex)
    if (!type %in% c("height","weight","bmi","muac","head")) {
        stop("Unknown type, known types are 'height', 'weight', 'bmi', 'muac' or 'head'!")
    }
    res = c()
    tabs=list()
    for (s in c("boys","girls")) {
        tabs[[s]] = this$ref_table(s,type)
    }
    if (length(x)>1) {
        if (length(age)==1) {
            age=rep(age,length(x)) 
            sex=rep(sex,length(x))
        }
    }
    sex=gsub("ma?l?e?","boys",sex)
    sex=gsub("fe?m?a?l?e?","girls",sex)
    sex[!(sex %in% c("girls","boys"))]=NA
    for (i in 1:length(x)) {
        if (is.na(sex[i])) {
            res=c(res,NA)
            next
        }
        tab=tabs[[sex[i]]]
        idx0=which(tab$Age==age[i])
        if (length(idx0)==0) {
            idx1=which(tab$Age<age[i])
            idx1=idx1[length(idx1)]
            idx2=which(tab$Age>age[i])[1]
            if (!is.numeric(idx1) | !is.numeric(idx2)) {
                res=c(res,NA)
                next
            }
            r=(tab$Age[idx2]-age[i])/(tab$Age[idx2]-tab$Age[idx1])
            M=tab[idx1,'M']*r+tab[idx2,'M']*(1-r)
            if (type %in% c("weight","bmi","muac","head")) {
                L=tab[idx1,'L']*r+tab[idx2,'L']*(1-r)
                S=tab[idx1,'S']*r+tab[idx2,'S']*(1-r)
                z=(((x[i]/M)^L)-1)/(L*S)
                res=c(res,z)
            } else {
                SD=tab[idx1,'SD']*r+tab[idx2,'SD']*(1-r)
                z = (x[i]-M)/SD
                res=c(res,z)
            }
        } else {
            M=tab[idx0,'M']
            if (any(type %in% c("weight","bmi","muac","head"))) {
                L=tab[idx0,'L']
                S=tab[idx0,'S']
                z=(((x[i]/M)^L)-1)/(L*S)
                res=c(res,z)
            } else {
                SD=tab[idx0,'SD']
                z = (x[i]-M)/SD
                res=c(res,z)
            }
        }
    }
    return(res)
}

#' \name{athi$report_pvalue}
#' \alias{athi$report_pvalue}
#' \alias{athi_report_pvalue}
#' \title{ p-value reporting for papers }
#' \description{
#'     Function for reporting a p-value either giving the three alpha thresholds, 
#'   <0.05, <0.01, or <0.001 or using the star syntax. 
#' }
#' \usage{ athi_report_pvalue(x,star=FALSE) }
#' \arguments{
#'   \item{x}{numerical p-value, vectors are as well possible}
#'   \item{star}{boolean, should the one-three star syntax be used, default: FALSE}
#' }
#' \details{
#'     This function can be used to report p-values in papers.
#' }
#' \value{scalar or vector of character strings}
#' \examples{
#'   athi$report_pvalue(1/10000)
#'   athi$report_pvalue(1/10000,star=TRUE)
#'   athi$report_pvalue(0.02,star=TRUE)
#'   athi$report_pvalue(0.12,star=TRUE)
#'   athi$report_pvalue(c(0.001,0.01,0.3,0.02))
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'

athi$report_pvalue <- function (x,star=FALSE) {
    p.val=x
    if (length(p.val) > 1) {
        return(as.character(lapply(p.val,athi$report_pvalue)))
    }
    if (p.val <0.001 & star) {
        return('***')
    } else if (p.val <0.001) {
        return('p<0.001')
    } else if (p.val <0.01 & star) {
        return('**')
    } else if (p.val <0.01) {
        return('p<0.01')
    } else if (p.val <0.05 & star) {
        return('*')
    } else if (p.val <0.05) {
        return('p<0.05')
    } else if (star) {
        return("n.s.")
    } else {
        return(sprintf("p=%.2f",p.val))
    }   
}   

#' \name{athi$impute}
#' \alias{athi$impute}
#' \alias{athi_impute}
#' \title{ impute missing values }
#' \description{
#'   Replaces missing values with a reasonable guess by different imputation methods.
#' }
#' \usage{ athi_impute(x,method="rpart",k=5,cor.method="spearman") }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{method}{
#'     character string, the method used for replacing missing values, either 'mean', 
#'      'median', 'rpart' or 'knn', default: 'rpart'
#'   }
#'   \item{k}{
#'     for method 'knn' number of nearest neighbors to use, default: 5
#'   } 
#'   \item{cor.method}{
#'     method to use in 'knn' imputation for using to create distance matrix
#'   }
#' }
#' \details{
#'   This method aids in replacing missing values with a reasonable guess by different imputation methods such as 
#'   the simple and not recommended methods mean and median, where NA's are replaced with the 
#'   mean or median for this variable or the more recommended methods using rpart decision trees
#'   or knn using a correlation distance based k-nearest neighbor approach. 
#'   The rpart method can be as well used to replace missing values for categorical variables.
#'   In case of median and mean imputations for categorical variables the modus is used, 
#'   so missing values are replaced with the most often category. This is rarely reasonable.
#' }
#' \value{depending on the input either a data frame or matrix with NA's replaced by imputed values}
#' \examples{
#'   data(iris)
#'   ir=athi$introNAs(iris[,1:4],prop=0.1)
#'   summary(ir)
#'   ir=athi$impute(ir)
#'   summary(ir)
#'   ir=iris
#'   ir[1,3]=NA; ir[2,4]=NA; ir[c(1,3),5]=NA
#'   head(ir)
#'   head(athi$impute(ir,method="rpart"))
#' }
#' 

athi$impute <- function (x,method="rpart",k=5,cor.method="spearman")  {   
    if (method %in% c("mean","median")) {
        for (i in 1:ncol(x)) {
            # integer is as well numeric :) so 
            if (is.numeric(x[,i])) {
                idx=which(is.na(x[,i]))
                if (method == "mean") {
                    x[idx,i]=mean(x[,i],na.rm=TRUE)
                } else if (method == "median") {
                    x[idx,i]=stats::median(x[,i],na.rm=TRUE)
                }  
            } else {
                # TODO: modus (?)
                warning(paste("Only numerical columns can be imputed with mean and median! Column",colnames(x)[i], "is however non-numeric!"))
            }
        }
    } else if (method == "rpart") {
        # TODO: refinement for many variables, 
        # take only variables with high absolute correlation
        # into account if more than 10 variables take top 10
        data=x
        idata=data
        for (i in 1:ncol(data)) {
            idx = which(!is.na(data[,i]))
            if (length(idx) == nrow(data)) {
                next
            }
            if (is.factor(data[,i])) { 
                model=rpart(formula(paste(colnames(data)[i],"~.")), 
                            data=as.data.frame(data[idx,]),
                            method="class")
                x2 = predict(model,newdata=as.data.frame(data[-idx,]),
                             type="class")
            } else {
                model=rpart(formula(paste(colnames(data)[i],"~.")), 
                            data=as.data.frame(data[idx,]))
                x2 = predict(model,newdata=as.data.frame(data[-idx,]))
            }

            idata[-idx,i]=x2
        }
        return(idata)
    } else if (method == "knn") {
        if (ncol(x) < 4) {
            stop("knn needs at least 4 variables / columns")
        }
        data.imp=x
        D=as.matrix(1-((cor(t(data.imp),use="pairwise.complete.obs")+1)/2))
        #D=as.matrix(1-cor(t(data.imp),use="pairwise.complete.obs"method=cor.method))
        for (i in 1:ncol(x)) {
            idx=which(is.na(x[,i]))
            idxd=which(!is.na(x[,i]))
            for (j in idx) {
                idxo=order(D[j,])
                idxo=intersect(idxo,idxd)
                mn=mean(x[idxo[1:k],i])
                data.imp[j,i]=mn
            }
        }
        return(data.imp)
    } else {
        stop("Unknown method, choose either mean, median, knn or rpart")
    } 
    return(x)
}
#' \name{athi$introNAs}
#' \alias{athi$introNAs}
#' \alias{athi_introNAs}
#' \title{ introduce missing values into data frames or matrices }
#' \description{
#'   Introduces NA's into the given data frame or matrix with a specified proportion.
#' }
#' \usage{ athi_introNAs(x,prop=0.05) }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{prop}{
#'     proportion of data where NA's should be introduced randomly, default: 0.05 (5 percent)
#'   }
#' }
#' \value{depending on the input either a data frame or matrix some NA's}
#' \examples{
#'   data(iris)
#'   ir=athi$introNAs(iris[,1:4],prop=0.1)
#'   summary(ir)
#'   dim(ir)
#'   apply(ir,2,function (x) { length(which(is.na(x))) })
#' }
#' 

athi$introNAs <- function (x,prop=0.05) {
    df=FALSE
    # changed class(x)[1] == "data.frame" to is.data.frame(x)
    if (is.data.frame(x)) {
         # TODO: deal with different datatypes in 
         # different columns with data frames
         dts=c()
        for (i in 1:ncol(x)) {
          dts=c(dts,class(x[,i]))
        }
        x=as.matrix(x)
        df=TRUE
    }
    n=as.integer(nrow(x)*ncol(x)*prop)
    idx=sample(1:(nrow(x)*ncol(x)),n)
    x[idx]=NA
    if (df) {
        x=as.data.frame(x)
        for (i in 1:ncol(x)) {
            if (dts[i]=="factor") {
                x[,i]=as.factor(x[,i])
            } else {
                class(x[,i])=dts[i]
            }
        }
    }
    return(x)
}    
#' \name{athi$norm}
#' \alias{athi$norm}
#' \alias{athi_norm}
#' \title{Normalizes given data frame or matrix using the choosen method}
#' \description{
#'   Introduces NA's into the given data frame or matrix with a specified proportion.
#' }
#' \usage{ athi_norm(x,method="z",ties.method="average") }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#'   \item{method}{the method used for normalization, either 'z' for z-score, 
#'      'uv' for unit variance, 'fs' for feature scaling within 0 and 1 and 
#'      'q' for quantile normalization, 'mp' for median polish, default: 'z'
#'   }
#'   \item{ties.method}{if normalization is `(q)antile` how ties should be handled, 
#'      default `average` which leads to slightly different scales (default in preprocessCore::normalize.quantile, whereas `random` gives 
#'      the same scales for all columns, this leads as well to different values for samples which had 
#'      originally the same values, default: 'average'
#'   }
#' }
#' \value{depending on the input either a data frame or matrix with normalized values}
#' \examples{
#'   data(iris)
#'   ir=athi$norm(iris[,1:4],method="uv")
#'   apply(ir,2,sd)
#'   summary(ir)
#'   ir2=athi$norm(iris[,1:4],method="fs")
#'   boxplot(ir2)
#'   summary(athi$norm(iris[,1:4],method="q"))
#'   # NA's are handled as well
#'   ir=iris[,1:4]
#'   ir[2,3]=NA
#'   ir[1,4]=NA
#'   ir[3,2]=NA
#'   head(ir)
#'   head(athi$norm(ir,method="q"))
#'   summary(athi$norm(ir,method="q"))
#'   sdata=read.table(text="4 3 6 4 7
#' 8 1 10 5 11
#' 6 2 7 8 8
#' 9 4 12 9 12
#' 7 5 9 6 10
#' ")
#' athi$norm(sdata,method="mp")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }


athi$norm <- function (x,method="z",ties.method="average")  {
    this=athi
    if (method %in% c("z","fs","uv")) {
        for (i in 1:ncol(x)) {
            if (is.numeric(x[,i])) {
                if (method == "z") {
                    x[,i]=(x[,i]-mean(x[,i],na.rm=TRUE))/sd(x[,i],na.rm=TRUE)
                } else if (method == "uv") {
                    x[,i]=x[,i]/sd(x[,i])
                } else if (method == "fs") {
                    x[,i]=(x[,i]-min(x[,i],na.rm=TRUE))/(max(x[,i],
                                                             na.rm=TRUE)-min(x[,i],na.rm=TRUE)) 
                } 
            } 
        } 
    } else if (method %in% c("mp")) { 
        x=x-medpolish(x,trace.iter=FALSE)$residuals
    } else if (method %in% c("q", "qn","quantiles")) {
        # TODO: handling of NA's
        if (any(is.na(x))) {
            # replace NA's with mean
            x2=this$impute(x,method="knn")
            svals=apply(apply(x2,2,sort),1,mean) # vector
            rmtx=apply(x2,2,rank,ties.method=ties.method) # matrix
            for (i in 1:ncol(x2)) {
                x2[,i]=svals[rmtx[,i]]
                # at the end only change non-NA's
                x[!is.na(x[,i]),i]=x2[!is.na(x[,i]),i]
            }
        } else {
            # no NA's handling
            # 4-5 lines of cod, 
            # Should we really need to install a Bioconductor package 
            # for this functionality?? Yes, the Bioconductor package is in C, so faster!
            # But ... 
            svals=apply(apply(x,2,sort),1,mean) # vector
            rmtx=apply(x,2,rank,ties.method=ties.method) # matrix
            for (i in 1:ncol(x)) {
                x[,i]=svals[rmtx[,i]]
            }
        }
    }  else  {
        stop("Unknown method, choose one of: z, uv, fs, mp  or q")
    }
    return(x)    
}

#' \name{athi$randomize}
#' \alias{athi$randomize}
#' \alias{athi_randomize}
#' \title{Randomize data frame or matrix columns}
#' \description{
#'   This function can be used to randomize the data within the same column.
#' }
#' \usage{ athi_randomize(x) }
#' \arguments{
#'   \item{x}{
#'     either a matrix or data frame
#'   }
#' }
#' \value{depending on the input either a data frame or matrix with randomized values}
#' \examples{
#' data(iris)
#' round(cor(iris[,1:4]),2)
#' round(cor(athi$randomize(iris[,1:4])),2)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'

athi$randomize <- function (x) {
    for (i in 1:ncol(x)) {
        x[,i]=sample(x[,i])
    }
    return(x)
}

athi_box_plot = athi$box_plot
athi_cdist = athi$cdist
athi_cohensD = athi$cohensD
athi_cohensW = athi$cohensW
athi_corr = athi$corr
athi_cor_plot = athi$cor_plot
athi_df2md = athi$df2md
athi_eta_squared = athi$eta_squared
athi_impute = athi$impute
athi_introNAs = athi$introNAs
athi_lm_plot = athi$lm_plot
athi_mds_plot = athi$mds_plot
athi_ref_score = athi$ref_score
athi_ref_table = athi$ref_table
athi_report_pvalue = athi$report_pvalue
athi_norm = athi$norm
athi_randomize = athi$randomize
