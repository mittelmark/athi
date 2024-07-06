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
#'   \item{\link[athi:athi_assoc_plot]{athi$assoc_plot(x,legend=NULL,shade=TRUE)}}{extended version of the assocplot with main statistic and Pearson residuals (plot)}
#'   \item{\link[athi:athi_bootstrap]{athi$bootstrap(x,FUN=NULL,n=1000,...)}}{resampling for the given data set and function (stats)}
#'   \item{\link[athi:athi_box_plot]{athi$box_plot(x,y=NULL)}}{extended version of the boxplot with main statistic values on top (plot)}
#'   \item{\link[athi:athi_chr2ord]{athi$chr2ord(x,map)}}{map characters or factors to numbers with comfort (data)}
#'   \item{\link[athi:athi_cdist]{athi$cdist(x,method="spearman",type="abs")}}{calculate correlation distances (data)}
#'   \item{\link[athi:athi_cihist]{athi$cihist(x,conf.level=0.95)}}{ draw a histogram density line with confidence band (plot)}
#'   \item{\link[athi:athi_cohensD]{athi$cohensD(x,g,paired=FALSE)}}{effect size for difference between two means (stats)}
#'   \item{\link[athi:athi_cohensW]{athi$cohensW(x,p=NULL)}}{effect size for categorical data (stats)}
#'   \item{\link[athi:athi_corr]{athi$corr(x,method="pearson",use="pairwise.complete.obs")}}{pairwise correlations and their statistics (stats)}
#'   \item{\link[athi:athi_cor_plot]{athi$cor_plot(x,y,method="pearson",...)}}{extended version of xy-plot with main statistics (plot)}
#'   \item{\link[athi:athi_corr_plot]{athi$corr_plot(x,...)}}{plot matrices with pairwise correlations (plot)}
#'   \item{\link[athi:athi_df2md]{athi$df2md(x,caption='',rownames=TRUE)}}{print a matrix or data frame as a Markdown table (data)}
#'   \item{\link[athi:athi_drop_na]{athi$drop_na(x,cols=NULL)}}{drop rows if the given columns contain NAs in this row for certain columns (data)}
#'   \item{\link[athi:athi_epsilon_squared]{athi$epsilon_squared(x,y)}}{effect size for Kruskal test (stats)}
#'   \item{\link[athi:athi_eta_squared]{athi$eta_squared(x,y=NULL)}}{effect size for ANOVA (stats)}
#'   \item{\link[athi:athi_fmt]{athi$fmt(x,...)}}{formatted string output (data)}
#'   \item{\link[athi:athi_impute]{athi$impute(x,method="rpart",k=5,cor.method="spearman")}}{impute missing values (data)}
#'   \item{\link[athi:athi_input]{athi$input(prompt)}}{readline replacement for scripts (programming)}
#'   \item{\link[athi:athi_introNAs]{athi$introNAs(x,prop="0.05")}}{introduce missing values (data)}
#'   \item{\link[athi:athi_kroki]{athi$kroki(text,mode="ditaa",...)}}{draw diagrams and flowcharts using kroki webservice (plot)}
#'   \item{\link[athi:athi_lm_plot]{athi$lm_plot(x,y=NULL,data=NULL,...)}}{plot a linear model with confidence intervals (plot)}
#'   \item{\link[athi:athi_mi]{athi$mi(x,y=NULL,norm=FALSE)}}{mutual information (stats)}
#'   \item{\link[athi:athi_mds_plot]{athi$mds_plot(x,method="euclidean",...)}}{plot a multidimensional scaling (plot)}
#'   \item{\link[athi:athi_norm]{athi$norm(x,method="z",ties.method="average")}}{normalize data (data)}
#'   \item{\link[athi:athi_pastel]{athi$pastel(n)}}{create up to 20 pastel colors (plot)}
#'   \item{\link[athi:athi_pca_biplot]{athi$pca_biplot(x)}}{improved biplot for pca objects (plot)}
#'   \item{\link[athi:athi_pca_oncor]{athi$pca_oncor(x)}}{perform a PCA on a square (correlation) matrix (stats)}
#'   \item{\link[athi:athi_pca_pairs]{athi$pca_pairs(x)}}{improved pairs plot for pca objects (plot)}
#'   \item{\link[athi:athi_pca_plot]{athi$pca_plot(x)}}{improved screeplot for pca objects (plot)}
#'   \item{\link[athi:athi_randomize]{athi$randomize(x)}}{randomize column data within matrix or data frame (data)}
#'   \item{\link[athi:athi_ref_score]{athi$ref_score(x,age,sex,type)}}{reference score for the given age, sex and type (data)}
#'   \item{\link[athi:athi_ref_table]{athi$ref_table(sex,type)}}{reference table for WHO for the given sex and measure type (daa)}
#'   \item{\link[athi:athi_report_pvalue]{athi$report_pvalue(p,star=FALSE)}}{report a p-value using the significance thresholds (stats)}
#'   \item{\link[athi:athi_sem]{athi$sem(x,na.rm=TRUE)}}{standard error of the mean (stats)}
#'   \item{\link[athi:athi_smartbind]{athi$smartbind(x,y)}}{combine two data frame even if the have different column names (data)}
#'   \item{\link[athi:athi_textplot]{athi$textplot(x)}}{write the data for a data frame or matrix into a plot (plot)}
#'   \item{\link[athi:athi_untab]{athi$untab(x)}}{expand a contingency table to a data frame one item per row (data)}
#'   \item{\link[athi:athi_venn]{athi$venn(x)}}{Venn diagram for logical relations between two and three sets (plot)}
#'   \item{\link[athi:athi_wilcoxR]{athi$wilcoxR(x,y)}}{effect size r, for a wilcox test object (stats)}
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

#' \name{athi$assoc_plot}
#' \alias{athi$assoc_plot}
#' \alias{athi_assoc_plot}
#' \title{ create an association plot with Pearson residual coloring }
#' \description{
#'   This function updates the standard assocplot function from the graphics package 
#'   with the ability to display residual colors. In blue and red are shown groups with 
#'   residuals above +4 or below -4 in light colors are shown residuals between 2 and 4 for positive and -4 and -2 for negative residuals.
#' }
#' \usage{ athi_assoc_plot(x,legend=TRUE,shade=TRUE,...) }
#' \arguments{
#'    \item{x}{contingency table}
#'    \item{legend}{should the residual table and the statistical values be shown at the bottom, default: TRUE}
#'    \item{shade}{should residuals being colored, default: ZTUE}
#'    \item{\ldots}{other arguments delegated to the default assocplot function}
#' }
#' \value{NULL}
#' \examples{
#'  x <- margin.table(HairEyeColor, c(1, 2))
#' athi$assoc_plot(x)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_cor_plot]{athi$cor_plot}, \link[athi:athi_box_plot]{athi$box_plot}
#' }
#' 

athi$assoc_plot <- function (x,legend=TRUE,shade=TRUE,...) {
    # https://stackoverflow.com/questions/38732663/how-to-insert-expression-into-the-body-of-a-function-in-r
    funins <- function(f, expr = expression(x<-2*x), after=1) {
        body(f)<-as.call(append(as.list(body(f)), expr, after=after))
        f
    }
    cols=c('#CF3761','#E18E9E','#E0E0E0','#96A2DF','#4267E0')
    expr=expression({
        # DG: changed for shade
        residuals=chisq.test(x)$residuals
        cols=c('#CF3761','#E18E9E','#E0E0E0','#96A2DF','#4267E0')
        resis=c()
        # R plots from top right to lower left ...
        # we rearrange the colors
        for (c in ncol(x):1) {
            resis=c(resis,residuals[,c])
        }
        acols=cols[cut(resis,
                       breaks=c(-Inf,-4,-2,2,4,Inf),
                       labels=c(1,2,3,4,5))]
        rect(z[, 1] - e/2, z[, 2], z[, 1] + e/2, z[, 2] + d, col = acols)
    })
    if (shade) {
       if (legend) {
           opar=par(mai=c(1.8,0.8,0.8,0.4))
           ct=chisq.test(x)
           w=paste("(w = ",round(athi$cohensW(x),2),",",athi$report_pvalue(ct$p.value),")",sep="")
       }
       g <- funins(graphics::assocplot, expr,after=length(body(graphics::assocplot)))
       g(x,...)
       if (legend) {
           par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
           plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
           legend(c("> 4 ","4 .. 2","2 .. -2", "-2 .. -4", " < -4"),
                  x="bottom",cex=1.2,fill=rev(cols),bty='n',title=paste("Pearson Residuals ",w),
                  xpd=TRUE,horiz=TRUE,box.lwd=0,inset=c(0,0))
           par(opar)
       }
    } else {
       graphics::assocplot(x,...)
   }
    
}
#' \name{athi$bootstrap}
#' \alias{athi$bootstrap}
#' \alias{athi_bootstrap}
#' \title{ Perform a resampling for the given data set and function. }
#' \description{
#'   The function allows you to perform a resampling method without replacement to perform
#'   a boostrap analysis for instance to cmpute a p-value or a confidence interval.
#' }
#' \usage{ athi_bootstrap(x,FUN=NULL,n=1000,...) }
#' \arguments{
#'    \item{x}{a vector, a data frame or a matrix}
#'    \item{FUN}{function handling the given data set type but performing before executing  FUN a sampling with replacement, please note that the function must return a scalar value}
#'    \item{n}{ number of resamplings to perform, default: 100}
#'    \item{\ldots}{remaining arguments are delegated to the given function `FUN`}
#' }
#' \value{vector with the resampled values from the given function}
#' \examples{
#'   rn=rnorm(100,mean=10,sd=2)
#'   t.test(rn)$conf.int
#'   vals=athi$bootstrap(rn,FUN=mean)
#'   summary(vals)
#'   quantile(vals,c(0.025,0.975)) # 95% CI is very close
#'   # confidence interval for spearman correlation
#'   cor.test(swiss[,1],swiss[,2],method="spearman")
#'   vals=athi$bootstrap(swiss[,c(1,2)],
#'        FUN=function(x) cor(x[,1],x[,2],method="spearman"))
#'   summary(vals)
#'   quantile(vals,c(0.025,0.975)) # 95% CI shows insignifcant
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}
#' }
#' 
#'
athi$bootstrap <- function (x,FUN=NULL,n=1000,...) {
    if (class(FUN)!="function") {
        stop("Error: Argument FUN with a function is missing!")
    }
    vals=c()
    for (i in 1:n) {
        if (is.matrix(x) | is.data.frame(x))  {
            idx=sample(1:nrow(x),nrow(x),replace=TRUE)
            vals=c(vals,FUN(x[idx,],...))
        } else {
            idx=sample(1:length(x),length(x),replace=TRUE)
            vals=c(vals,FUN(x[idx],...))
        }
    }
    return(vals)
}

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

#' \name{athi$chr2ord}
#' \alias{athi$chr2ord}
#' \alias{athi_chr2ord}
#' \title{ Map characters or factors to numbers }
#' \description{
#'     Create ordinal numerical variables out of character or factor variables based on a given mapping.
#' }
#' \usage{ athi_chr2ord(x,map) }
#' \arguments{
#'  \item{x}{character or factor vector}
#'  \item{map}{list with keys for the given x vector and numbers for the matching values}
#' }
#' \value{numerical values for the mapping}
#' \examples{
#' status=c("never","rare","often","always")
#' x=sample(status,100,replace=TRUE)
#' x=c(NA,x,NA)
#' table(x,useNA='ifany')
#' map=c(never=0, rare=1, often=2,always=3)
#' table(athi$chr2ord(x,map),useNA='ifany')
#' }

athi$chr2ord = function (x,map) {
   return(unlist(lapply(as.character(x),function(x) {
        if (is.na(x)) { return(NA) }
          return(map[[x]])
    }
   )))
}

#' \name{athi$cihist}
#' \alias{athi$cihist}
#' \alias{athi_cihist}
#' \title{Draw a histogram density line with confidence level}
#' \description{
#'    This is a utility function to draw a histogram with density 
#'   lines and the confidence level for that density line.
#' }
#' \usage{ athi_cihist(x, conf.level=0.95, legend=TRUE, xlab="Value", ylab="Density", ...) }
#' \arguments{
#'   \item{x}{
#'     a numerical vector
#'   }
#'   \item{conf.level}{
#'     The confidence level for the histogram, default 0.95
#'   }
#'   \item{legend}{
#'     Should a legend been drawn, default: TRUE
#'   }
#'   \item{xlab}{
#'     Label for the x-axis, default: "Value"   
#'   }
#'   \item{ylab}{
#'     Label for the y-axis, default: "Density"   
#'   }
#'   \item{\ldots}{
#'     Arguments delegated to the plot function
#'   }
#' }
#' \examples{
#'   set.seed(123)
#'   data <- rnorm(1000, mean = 0, sd = 1)
#'   athi$cihist(data,main="Histogram density line with 95\% confidence level")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'


athi$cihist <- function (x,conf.level=0.95,legend=TRUE,xlab="Value",ylab="Density",...) {
    dens <- density(x)

    # Calculate confidence interval (assuming normal distribution)
    ci <- 1.96 * sd(x) / sqrt(length(x))
    upper <- dens$y + ci
    lower <- dens$y - ci

    # Create the plot
    plot(dens, main = "Density Plot with 95% Confidence Interval", 
     xlab = xlab, ylab = ylab, ylim = c(0, max(upper)))

    # Add confidence interval
    polygon(c(dens$x, rev(dens$x)), c(lower, rev(upper)), 
            col = rgb(0.5, 0.5, 0.5, 0.5), border = NA)

    # Redraw the density line on top
    lines(dens, col = "blue", lwd = 2)

    # Add a legend
    if (legend) {
        legend("topright", legend = c("Density", "95% CI"), 
               col = c("blue", "gray"), lwd = c(2, 10), bty = "n")
    }

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
#'    \item lower - lower range of the 95 percent confidence interval (for method Pearson and Spearman)
#'    \item upper - upper range of the 95 percent confidence interval (for methods Pearson and Spearman)
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
            if (method == "spearman") {
                stderr = 1/sqrt(nrow(stats::na.omit(x[,c(i,j)]))-3)
                delta = 1.96*stderr
                mt.lower[i,j]=mt.lower[j,i]=tanh(atanh(rt$estimate) - delta)
                mt.upper[i,j]=mt.upper[j,i]=tanh(atanh(rt$estimate) + delta)
            } else if (method == "pearson") {
                mt.lower[i,j]=mt.lower[j,i]=rt$conf.int[1]
                mt.upper[i,j]=mt.upper[j,i]=rt$conf.int[2]
            }
            # TODO: Kendall ...
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
    plot(x~y,col=col,pch=pch,...);
    mtext(side=3,adj=1,r)
    if (grid) {
        grid()
        points(x~y,col=col,pch=pch,...)
    }
    abline(lm(x~y),col=col,lwd=2)
    box()
}
#' \name{athi$corr_plot}
#' \alias{athi$corr_plot}
#' \alias{athi_corr_plot}
#' \title{ visualize a correlation matrix }
#' \description{
#'     This function is plotting the pairwise correlations for the given
#'     symmetric correlation matrix.
#' }
#' \usage{ athi_corr_plot(mt,text.lower=TRUE, text.upper=FALSE,
#'                      pch=19,p.mat=NULL,alpha=0.05,
#'                     cex.sym=5,cex.r=1,cex.lab=1.4,...)}
#' \arguments{
#' \item{mt}{matrix with pairwise correlations}
#' \item{text.lower}{should in the lower diagonal the correlation coefficient be shown, default: TRUE}
#' \item{text.upper}{should in the upper diagonal the correlation coefficient be shown, default: FALSE}
#' \item{pch}{the plotting symbol for the correlations, default: 19}
#' \item{p.mat}{matrix with p-values to strike out insignificant p-values, default: NULL (not used)}
#' \item{alpha}{significance threshold for `p.mat`, default: 0.05}
#' \item{cex.sym}{character expansion for the correlation symbols, default: 5}
#' \item{cex.r}{character expansion for the r-values if _text.lower_ or _text.upper_ are set to TRUE, default: 1}
#' \item{cex.lab}{character expansion for the variable text labels, default: 1.4}
#' \item{\ldots}{other arguments delegated to the plot function}
#' }
#' \value{NULL}
#' \examples{
#' data(swiss)
#' sw=swiss
#' colnames(sw)=abbreviate(colnames(swiss),6)
#' options(warn=-1) # avoid spearman warnings
#' cr=athi$corr(sw,method='spearman')
#' athi$corr_plot(cr$estimate,cex.sym=8,text.lower=TRUE,
#'    cex.r=1.5,p.mat=cr$p.value)
#' options(warn=0)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr}
#' }
#' 

athi$corr_plot <- function (mt,text.lower=TRUE, text.upper=FALSE,
                      pch=19,p.mat=NULL,alpha=0.05,
                      cex.sym=5,cex.r=1,cex.lab=1.4,...) {
    if (class(p.mat)[1] == 'NULL') {
        p.mat=mt
        p.mat[]=0
    }
    yend=nrow(mt)+1
    xend=ncol(mt)+1
    plot(1,type="n",xlab="",ylab="",axes=FALSE,
         xlim=c(-0.2,xend),ylim=c(nrow(mt),0),...)
    text(1:(ncol(mt)),0.25,colnames(mt),cex=cex.lab)
    text(-0.2,1:nrow(mt),rownames(mt),cex=cex.lab,pos=4)
    cols=paste("#DD3333",rev(c(15,30, 45, 60, 75, 90, "AA","BB","CC","DD")),sep="")
    cols=c(cols,paste("#3333DD",c(15,30, 45, 60, 75, 90, "AA","BB","CC","DD"),sep=""))
    breaks=seq(-1,1,by=0.1)                  
    sym=identical(rownames(mt),colnames(mt))
    for (i in 1:nrow(mt)) {
        for (j in 1:nrow(mt)) {
            if (sym & i == j) {
                next
            }   
            coli=cut(mt[i,j],breaks=breaks,labels=1:20)
            if (i == j & !sym & text.lower) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }

            } else if (i < j & text.lower) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }

            } else if (i > j & text.upper) {
                text(i,j,round(mt[i,j],2),cex=cex.r)
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.r*2)
                }
            } else {
                points(i,j,pch=pch,cex=cex.sym,col=cols[coli])
                if (p.mat[i,j]>alpha) {
                    text(i,j,"x",cex=cex.sym*0.3)
                }
            }
        }
    }
}

#' \name{athi$cv}
#' \alias{athi$cv}
#' \alias{athi_cv}
#' \title{ coefficient of variation}
#' \description{
#' Calculate the coefficient of variation.
#' }
#' \usage{ athi_cv(x,na.rm=FALSE) }
#' \arguments{
#' \item{x}{vector with positive numerical values}
#' \item{na.rm}{should NA's be removed, default: FALSE}
#' }
#' \value{numerical value for the coefficient of variation}
#' \examples{
#' cv=athi$cv
#' cv(rnorm(20,mean=100,sd=4))
#' cv(c(1,2,3,4))
#' cv(c(1,2,3,4,NA))
#' cv(c(1,2,3,4,NA),na.rm=TRUE)
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}
#' }
#'

athi$cv <- function (x,na.rm=FALSE) {
    cv=100*sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
    return(cv)
}
#' \name{athi$epsilon_squared}
#' \alias{athi$epsilon_squared}
#' \alias{athi_epsilon_squared}
#' \title{ Effect sizes measure for Kruskal test }
#' \description{
#' Calculate the effect size epsilon-squared for variables of a kruskal.test
#' }
#' \usage{ athi_epsilon_squared(x,y) }
#' \arguments{
#' \item{x}{vector with numerical values}
#' \item{y}{vector with factor values}
#' }
#' \details{
#'  The function `athi$epsilon_squared` calculates the effect size for a Kruskal test.
#'  Cohen's rule of thumb for interpretation is: around 0.01 small, around 0.09 medium and around 0.25 or higher we have a large effect.
#'  You can convert Epsilon-squared to a Pearson r coefficient by using the sqrt of The Epsilon-square value.
#' 
#' Please note that these rules of thumb are not useful for highly dependent outcome 
#' variables (death for instance) these rules might not be useful and as well lower
#' values might be of practical relevance.
#' }
#' \value{numerical value, effect size Epsilon-squared}
#' \examples{
#' data(iris)
#' athi$epsilon_squared(iris$Sepal.Length,iris$Species)
#' # two factor example as well for wilcox.test possible
#' data(ToothGrowth)
#' athi$epsilon_squared(ToothGrowth$len,as.factor(ToothGrowth$dose))
#' # close to r-square of spearman!
#' cor(ToothGrowth$len,ToothGrowth$dose,method="spearman")^2
#' }
#' 
#' \seealso{
#'    \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD}, 
#'     \link[athi:athi_cohensW]{athi$cohensW}, \link[athi:athi_eta_squared]{athi$eta_squared}  
#' }
#' 

athi$epsilon_squared <- function (x,y) {
    if (class(y) %in% c("numeric","integer")) {
        H=unname(kruskal.test(list(x,y))$statistic)
        n=length(x[which(!is.na(x) & !is.na(y))])
    }  else {
        H=unname(kruskal.test(x ~ y)$statistic)
        n=sum(table(x,y)) # get rid of NAs
    }    
    es=H/((n^2-1)/(n+1))
    return(unlist(es))
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
#'  The function `athi$eta_squared` calculates the effect size for an ANOVA.
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
#'    \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD},
#'    \link[athi:athi_cohensW]{athi$cohensW}, \link[athi:athi_epsilon_squared]{athi$epsilon_squared}   
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
#' \usage{ athi_df2md(x,caption='', center=TRUE, rownames=TRUE) }
#' \arguments{
#'    \item{x}{matrix or data frame}
#'    \item{caption}{the caption for the table, it is just displayed below of the table, default: ''}
#'    \item{center}{should the table and the optional caption been center, default: TRUE}
#'    \item{rownames}{should  the rownames be displayed, default: TRUE}
#' }
#' \value{prints to stdout}
#' \examples{
#' data(swiss)
#' athi$df2md(head(swiss),caption="**Table X:** Swiss Data")
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#'


athi$df2md <- function(x,caption='', center=TRUE, rownames=TRUE) {
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
    if (center) {
        cat("<center>\n")
    }
    cat(fin)
    if (center) {
        cat("</center>\n")
    }

        
}

#' 
#' \name{athi$drop_na}
#' \alias{athi$drop_na}
#' \alias{athi_drop_na}
#' \title{ Removes all rows where any of the columns contain a NA. }
#' \description{
#'  In contrast to `na.omit`, the method `athi$drop_na` just checks the given columns 
#'  to delete rows which have any NA in these two columns in the given rows. 
#'  This mimics the `tidyr::drop_na` function.
#' }
#' \usage{ athi_drop_na(x,cols=NULL) }
#' \arguments{
#'    \item{x}{matrix or data frame}
#'    \item{cols}{the column names or ids, if not given all columns will be checked, default: NULL}
#' }
#' \value{returns data frame or matrix with rows containing NA's removed}
#' \examples{
#' data(iris)
#' ir=iris
#' ir[c(1,3),1]=NA
#' ir[2,2]=NA
#' ir[4,4]=NA
#' head(ir)
#' head(na.omit(ir)) # removes all rows with an NA somewhere
#' head(athi$drop_na(ir,1:2)) # just checks the first two columns
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#' 

athi$drop_na = function (x,cols=NULL) { 
    if (is.null(cols[1])) {
        cols=1:ncol(x)
    }
    idx=which(apply(!is.na(x[,cols]),1,all)); 
    return(x[idx,]) 
} 

#' 
#' \name{athi$fmt}
#' \alias{athi$fmt}
#' \alias{athi_fmt}
#' \title{ Python like formatting of strings using curly braces }
#' \description{
#'  This is a function for formatted output avoiding a lot of quotes
#'  during creation of strings based on variables. It mimics the Python format command.
#' }
#' \usage{ athi_fmt(x,...) }
#' \arguments{
#'    \item{x}{character string, usually with curly braces as place holders for the variables}
#'    \item{\ldots}{variable number of arguments used to replace the curly braces within x}
#' }
#' \value{returns the formatted string}
#' \examples{
#'   athi$fmt('I can say {} {}!',"Hello", "World")
#'   athi$fmt('I can say {2} {1}!',"World", "Hello")
#'   athi$fmt("pi is '{}'!",sprintf("\%3.5f",pi))
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#' 

athi$fmt = function (x,...) {
    args=list(...);
    if (class(args[[1]]) == 'list') {
        args=args[[1]]
    }
    for (i in 1:length(args)) {
        x=sub("\\{\\}",args[[i]],x)
        x=sub(paste("\\{",i,"\\}",sep=''),
                args[[i]],x)
    }
    return(x)
}

#' 
#' \name{athi$input}
#' \alias{athi$input}
#' \alias{athi_input}
#' \title{ Readline line but working as well in Rscripts non-interactivly.}
#' \description{
#'  Replacement for the readline function in non-interactive scripts. 
#'   As the readline function does only works in interactive mode we need an alternative.
#' }
#' \usage{ athi_input(prompt) }
#' \arguments{
#'    \item{prompt}{text displayed to ask for input of the user.character string, usually with curly braces as place holders for the variables}
#' }
#' \value{returns the entered string}
#' \examples{
#'   \dontrun{
#'    x = as.numeric(athi$input("Enter a number: "))
#'   }
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class} 
#' }
#' 
 

athi$input = function (prompt="Enter: ") {
    if (interactive() ){ 
        return(readline(prompt))
    } else {
        cat(prompt);
        return(readLines("stdin",n=1))
    }
}

#' \name{athi$kroki}
#' \alias{athi$kroki}
#' \alias{athi_kroki}
#' \title{ Create diagrams using the kroki online service. }
#' \description{
#'  This function creates an URL which can be easily embedded into Markdown code for displaying
#'   diagrams supported by the online tool at https://kroki.io
#'   There is as well an online diagram editor, see here https://niolesk.top/
#' }
#' \usage{ athi_kroki(text,filename=NULL,type="ditaa",ext="png",cache=TRUE,plot=FALSE,server="kroki") }
#' \arguments{
#' \item{text}{some diagram code,default: "A --> B" }
#' \item{filename}{some input file, either 'text' or 'file' must be given, default: NULL}
#' \item{type}{diagram type, supported is 'ditaa', 'graphviz', 'plantuml' and many others, see the kroki website, default: 'ditaa'}
#' \item{ext}{file extension, usally 'png', 'svg' or 'pdf', not all extensions support 'svg' or 'pdf', default: 'png'}
#' \item{cache}{should the image be cached locally using crc32 digest files in an 'img' folder, default: TRUE}
#' \item{plot}{should the image directly plotted, default: FALSE}
#' \item{server}{Which server to use, 'kroki' or 'plantuml', default: 'kroki'}
#' }
#' \examples{
#' \dontrun{
#' url1=athi$kroki('
#'   digraph g { 
#'   rankdir="LR";
#'   node[style=filled,fillcolor=beige];
#'   A -> B -> C ; 
#' }',
#' type="graphviz",server="kroki")
#' url2=athi$kroki("
#'   +---------+    +--------+
#'   |    AcEEF+--->+   BcFEE+
#'   +---------+    +--------+
#'   ",server="kroki")
#' url1
#' url2
#' }
#' }
#' 
#' 

athi$kroki <- function (text,filename=NULL,type="ditaa",ext="png",cache=TRUE,plot=FALSE,server="kroki") {
    if (!requireNamespace("tcltk")) {
        stop("For athi$kroki the package tcltk is required!")
    }
    # memCompress and openssl::base64_encode in R 
    # did not work always as expected
    # using good old Tcl
    tcltk::.Tcl("
    proc dia2kroki {text} {
        return [string map {+ - / _ = \"\"}  [binary encode base64 [zlib compress $text]]]
    }
    proc dia2puml {text {ext svg}} {
        set b64 ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/
        set pml 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz-_
        
        set lmapper [list]
        set i 0
        foreach char [split $b64 \"\"] {
            lappend lmapper $char
            lappend lmapper [string range $pml $i $i]
            incr i
        }
        # unclear while string range 2 end-4
        set b64 [string map $lmapper [binary encode base64 [string range [zlib compress [encoding convertto utf-8 $text]] 0 end]]]
        set uri https://www.plantuml.com/plantuml/$ext/~1$b64
        return $uri
    }
    ")
    if(plot & !requireNamespace("png",quietly=TRUE)) {
        stop("Error: Plotting kroki images requires library png!")
    }
    if (!is.null(filename)) {
        if (!file.exists(filename)) {
            stop(paste("Error: File",filename,"does not exists!"))
        } else {
            fin=file(filename,'r')
            text=readLines(fin,n=-1L)
            close(fin)
            text=paste(text,collapse="\n")
        }
    }
    if (grepl("\\s*@start",text)) {
        type="plantuml"
    }
    if (server == "plantuml") {
        if (type == "ditaa") {
            text = paste("@startditaa\n",text,"\n@endditaa\n",sep="")
        } 
        url = tcltk::tclvalue(tcltk::tcl("dia2puml",text,ext))
    } else {
        url = tcltk::tclvalue(tcltk::tcl("dia2kroki",text))
        url= paste("https://kroki.io",type,ext,url,sep="/")
    }
    if (cache) {
        if (!requireNamespace("digest",quietly=TRUE)) {
            stop("You need to install the digest library to cache images!")
        } 
        if (!dir.exists("img")) {
            dir.create("img")
        }
        filename=paste(digest::digest(url,"crc32"),".",ext,sep="")
        imgname=file.path("img",filename)
        if (!file.exists(imgname)) {
            utils::download.file(url,imgname,mode="wb")
        }
        if (plot) {
            img=png::readPNG(imgname)
            grid::grid.raster(img)
        } else {
            return(imgname)
        }
    } else {
        if (plot) {
            img=png::readPNG(url)
            grid::grid.raster(img)
        } else {
            return(url)
        }
    }
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

#' \name{athi$mi}
#' \alias{athi$mi}
#' \alias{athi_mi}
#' \title{ measure mutual information}
#' \description{
#'     Return the mutual information for two vectors, a matrix or data frame,  or a binned table.
#' }
#' \usage{ athi_mi(x,y=NULL,breaks=4,norm=FALSE) }
#' \arguments{
#'   \item{x}{either a binned table, a matrix or data.frame or a numerical vector}
#'   \item{y}{numerical vector, required only if x is not a binned table or matrix or data.frame}
#'   \item{breaks}{number of breaks to create a binned table if x and y are numerical vectors, default: 4}
#'   \item{norm}{if input is given should the matrix be normalized by dividing the off-diagonal values by the mutual information in the diagonals, so the self mutual information, default: FALSE}
#' }
#' \value{mutual information value as scalar if input is table or two vectors or as matrix if input is matrix or data.frame}
#' \examples{
#' rn1=rnorm(100,mean=10,sd=1);
#' rn2=rn1+0.5*rnorm(100)
#' cor(rn1,rn2) # high
#' cor(rn1,sample(rn2)) #random 
#' athi$mi(rn1,rn2) # high 
#' athi$mi(rn1,sample(rn2)) #random
#' athi$mi(rn1,rn2,breaks=4)
#' athi$mi(rn1,rn2,breaks=7)
#' data(swiss)
#' round(athi$mi(swiss),2)
#' round(athi$mi(swiss,norm=TRUE),2)
#'}
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_corr]{athi$corr}
#' }
#'

athi$mi = function (x,y=NULL,breaks=4,norm=FALSE) {
    if (is.matrix(x) | is.data.frame(x)) {
        if (ncol(x)==2) {
            return(athi$mi(x=x[,1],y=x[,2],breaks=breaks))
        } else {
            M=matrix(0,nrow=ncol(x),ncol=ncol(x))
            rownames(M)=colnames(M)=colnames(x)
            for (i in 1:(ncol(x)-1)) {
                for (j in i:ncol(x)) {
                    M[i,j]=M[j,i]=athi$mi(x[,i],x[,j],breaks=breaks)
                }   
            }
            # last cell
            M[ncol(x),ncol(x)]=athi$mi(x[,ncol(x)],x[,ncol(x)])
            if (norm) {
                M=M/diag(M)
            }
            return(M)
        }
    }
    if (!is.table(x)) {
        if (class(y)[1] != "NULL") {
            x=table(cut(x,breaks=breaks),cut(y,breaks=breaks))        
        } else {
            stop("if x is vector, y must be given as well")
        }
    }
    f1=x/sum(x)
    fx=rowSums(f1)
    fy=colSums(f1)
    fn=fx %o% fy
    f2=fn/sum(fn)
    LR = ifelse(f1>0,log(f1/f2),0)
    MI = sum(f1*LR)
    return(MI)
}

#' \name{athi$pastel}
#' \alias{athi$pastel}
#' \alias{athi_pastel}
#' \title{Create up to 20 pastel colors.}
#' \description{
#' This is an alternative color creation function for R versions before 3.6 where 
#' the function `hcl.colors` is not available.
#' }
#' \usage{ athi_pastel(n) }
#' \arguments{
#' \item{n}{number of colors requested, must be within 2 and 20}
#' }
#' \value{Vector of colors in RGB codes of requested length 'n'}
#' \examples{
#' athi$pastel(4)
#' par(mai=c(0.2,0.2,0.2,0.1))
#' plot(1:20,col=athi$pastel(20),cex=3,pch=15)
#' }
#' 
athi$pastel = function (n) {
    if(n > 20 |  n < 1) {
        stop("only between 1 and 20 colors can be given" ) 
    }
    pcols= c("#FFC5D0","#FDC8C3","#F6CBB7","#EDD0AE","#E2D4A8","#D4D8A7","#C5DCAB","#B6DFB4","#A8E1BF",
             "#9EE2CB", "#99E2D8","#9BE0E5","#A4DDEF","#B3D9F7","#C4D5FB","#D5D0FC","#E4CBF9","#F0C7F2",
             "#F9C5E9", "#FEC4DD")
             idx=seq(1,20,by=floor(20/n))
             return(pcols[idx])
}

#' \name{athi$pca_biplot}
#' \alias{athi$pca_biplot}
#' \alias{athi_pca_biplot}
#' \title{ Improved biplot for pca objects. }
#' \description{
#' The function `athi$pca_biplot` provides an improved biplot for
#' visualizing the pairwise scores of individual principal components of 
#' an object created using the function `prcomp`. In contrast to the default 
#' biplot function  this plot visualizes the data as points and not row numbers,
#' it allows to display groups using color codes and distribution ellipses.
#' }
#' \usage{ athi_pca_biplot(pca,pcs=c("PC1","PC2"),
#'                        pch=19,col='black',
#'                        arrows=TRUE,arrow.fac=1,
#'                        ellipse=FALSE,ell.fill=FALSE,xlab=NULL,ylab=NULL,...) }
#' \arguments{
#' \item{pca}{pca object of class `prcomp`, created using the function `prcomp`}
#' \item{pcs}{the components to plot, default: c('PC1','PC2')}
#' \item{pch}{plotting character, default: 19}
#' \item{col}{plotting color, default: black}
#' \item{arrows}{should loading arrows be displayed, default: TRUE}
#' \item{arrow.fac}{scaling factor for arrow length, default: 1}
#' \item{ellipse}{should 85 and 95 confidence intervals for the chisq distribution be shown. If this is shown colors for each group using the col argument must be given, default: FALSE}
#' \item{ell.fill}{should a filled 85 percent confidence interval be shown, colors will be used from the plotting color with opacity, default: FALSE}
#' \item{xlab}{custom xlab, if not given the PC name with variance in percent is shown, default: NULL}
#' \item{ylab}{custom ylab, if not given the PC name with variance in percent is shown, default: NULL}
#' \item{\ldots}{additional arguments delegated to the standard plot function}
#' }
#' \value{NULL}
#' \examples{
#' par(mai=c(0.8,0.8,0.2,0.6),mfrow=c(1,2))
#' data(iris)
#' pci=prcomp(iris[,1:4],scale=TRUE)
#' athi$pca_biplot(pci,col=rep(2:4,each=50),ellipse=TRUE,ell.fill=TRUE,
#'     arrow.fac=2.3,arrows=TRUE,main="biplot")
#' legend('topright',pch=19,col=2:4,levels(iris$Species))
#' # standard score plot
#' athi$pca_biplot(pci,col=rep(2:4,each=50),ellipse=FALSE,
#'    arrow.fac=2.3,arrows=FALSE,main="scoreplot")
#' }
#' 

athi$pca_biplot = function (pca,pcs=c("PC1","PC2"),
                       pch=19,col='black',
                       arrows=TRUE,arrow.fac=1,
                       ellipse=FALSE,ell.fill=FALSE,xlab=NULL,ylab=NULL,...) {
    if (missing("xlab")) {
        xlab=paste(pcs[1]," (", round(summary(pca)$importance[2,pcs[1]]*100,1),"%)",sep="")
    } 
    if (missing("ylab")) {
        ylab=paste(pcs[2]," (", round(summary(pca)$importance[2,pcs[2]]*100,1),"%)",sep="")
    } 
    plot(pca$x[,pcs[1]],pca$x[,pcs[2]],pch=pch,col=col,type="n",xlab=xlab,ylab=ylab,...)
    abline(h=0,lty=2)
    abline(v=0,lty=2)    
    if (ellipse) {
        if (length(col)!= nrow(pca$x)) {
            stop("colors must have sam elength as data points")
        }
        ell.col=col
        i=1
        for (cl in names(table(ell.col))) {
            C=cov(pca$x[ell.col==cl,c(pcs[1],pcs[2])])    # Covarianz-Matrix C bestimmen
            d85=qchisq(0.85, df = 2)     # 85% - Faktor , um die Ellipse zu skalieren
            M=colMeans(pca$x[ell.col==cl,c(pcs[1],pcs[2])]) #   Mittelwerte (Zentrum) des Clusters
            el=ellipsoidPoints(C, d85, loc=M)  # Ellipsen-Punkte aus C und M berechnen
            if (ell.fill) {
                colfill=paste(rgb(t(col2rgb(cl))/255),"33",sep="")
                polygon(el,col=colfill,border=NA)
                i=i+1
                next
            }
            lines(el,col=cl,lwd=1.5,lty=2)    #  Ellipse als geschlossene Linies zeichnen
            d95=qchisq(0.95, df = 2)     # 85% - Faktor , um die Ellipse zu skalieren
            el=ellipsoidPoints(C, d95, loc=M)  # Ellipsen-Punkte aus C und M berechnen
            lines(el,col=cl,lwd=1.5,lty=1)    #  Ellipse als geschlossene Linies zeichnen                        

        }
    }
    points(pca$x[,pcs[1]],pca$x[,pcs[2]],pch=pch,col=col,...)
    if (arrows) {
        loadings=pca$rotation
        arrows(0,0,loadings[,pcs[1]]*arrow.fac,loadings[,pcs[2]]*arrow.fac,
               length=0.1,angle=20,col='black')
        text(loadings[,pcs[1]]*arrow.fac*1.2,loadings[,pcs[2]]*arrow.fac*1.2,
             rownames(loadings),col='black',font=2)
    }

}

#' \name{athi$pca_oncor}
#' \alias{athi$pca_oncor}
#' \alias{athi_pca_oncor}
#' \title{Perform a PCA on a correlation matrix.}
#' \description{
#' The function `athi$pca_oncor` does a PCA using eigenvector eigenvalue decomposition
#'   on a correlation matrix. PCA usually performs Pearson correlation internally what
#'   leads to a highly outlier sensitive analysis. If the user decides
#'   to use a method like Spearman or even bi-seriell, polychoric or for nominal data
#'   effect size measures like Cohen's W this method here can be used. Note that this
#'   does not return new coordinates for the sample as the sample contribution is lost in the
#'   correlation matrix. The method might however be used to check if the results between
#'   Pearson and Spearman PCA are similar or does  outliers lead to a completly different result.
#' }
#' \usage{ athi_pca_oncor(x) }
#' \arguments{
#' \item{x}{a symmetric matrix usually with pairwise correlations}
#' }
#' \value{PCA like list object with components sd and rotation}
#' \examples{
#' data(USArrests)
#' C=cor(USArrests)
#' athi$pca_oncor(C)
#' D=cor(USArrests,method="spearman") 
#' athi$pca_oncor(D)
#' }
#' 
athi$pca_oncor <- function (x) {
    ev=eigen(x)
    res=list(rotation=ev$vectors,sdev=sqrt(ev$values))
    colnames(res$rotation)=paste("PC",1:ncol(x),sep="")
    rownames(res$rotation)=rownames(x)
    return(res)
             
}
#' \name{athi$pca_pairs}
#' \alias{athi$pca_pairs}
#' \alias{athi_pca_pairs}
#' \title{Improved pairs plot for pca objects.}
#' \description{
#'   The function `athi$pca_pairs` provides an improved pairs plot for
#'   visualizing the pairwise scores of the individual components of an analyses 
#'   using the function `prcomp`. In contrast to the default `pairs` function 
#'   this plot visualizes in the diagonal as well the variances and 
#'   a density line for the component scores.
#' }
#' \usage{ athi_pca_pairs(pca,n=10,groups=NULL, col='black',pch=19,legend=FALSE,...) }
#' \arguments{
#' \item{pca}{pca object which was created using the function `prcomp`.}
#' \item{n}{maximal number of components to visualize, default: 10}
#' \item{groups}{vector with classes having the same length than the inout matrix for prcomp has rows, default: NULL}
#' \item{col}{colors for the plotting, character, default: 'black'}
#' \item{pch}{plotting, symbol, default: 19}
#' \item{legend}{should the legend be displayed on top, default: FALSE}
#' \item{\ldots}{additional arguments delegated to the standard `pairs` function}
#' }
#' \value{NULL}
#' \examples{
#' data(iris)
#' pci=prcomp(iris[,1:4],scale=TRUE)
#' athi$pca_pairs(pci,pch=15,groups=iris[,5],
#'    legend=TRUE,oma=c(5,4,4,4),col=as.numeric(iris[,5])+1)
#' }
#' 

athi$pca_pairs = function (pca,n=10,groups=NULL, col='black',pch=19,legend=FALSE,...) {
    athi$N = 1
    if (n>ncol(pca$x)) {
        n=ncol(pca$x)
    }
    pst=FALSE
    if (class(groups) != "NULL" & length(col) != length(groups)) {
        coln=length(levels(as.factor(groups)))
        cols=athi$pastel(coln)
        col=cols[as.numeric(as.factor(as.character(groups)))]
        pst=TRUE
    }
    panel.text = function (x,...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5,0.5,
             paste(sprintf("%.1f",summary(pca)$importance[2,athi$NN]*100),"%",
                   sep=""),cex=1.5)
        ds=density(pca$x[,athi$N],na.rm=TRUE)
        ds$x=ds$x-min(ds$x)
        ds$x=ds$x/max(ds$x)
        ds$y=(ds$y/max(ds$y)*0.3)
        polygon(ds,col='grey80')
        athi$N = athi$N + 1
    }
    pairs(pca$x[,1:n],diag.panel=panel.text,col=col,pch=pch,...)
    if (legend && class(groups) != "NULL") {
        opar=par()
        options(warn=-1)
        par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
        plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
        if (pst) {
            leg.cols=athi$pastel(coln)[as.numeric(as.factor((levels(as.factor(groups)))))]
        } else {
            cols=col
            names(cols)=as.character(groups)
            lcol=cols[unique(names(cols))]
            leg.cols=as.numeric(lcol)
        }
        legend('bottom', levels(as.factor(groups)), xpd = TRUE, 
               horiz = TRUE, inset = c(0,0), 
               bty = "n", pch = pch, col = leg.cols, cex = 1.2)
        
        par(opar)
    }
}
#' \name{athi$pca_plot}
#' \alias{athi$pca_plot}
#' \alias{athi_pca_plot}
#' \title{Improved bar or screeplot for pca objects.}
#' \description{
#' The function `athi$pca_plot` provides an improved bar- or screeplot for
#' visualizing the variances of the individual components of an analyses 
#' using the function _prcomp_. In contrast to the default plot function 
#' this plot visualize cumulative and individual variances in percent.
#' }
#' \usage{ athi_pca_plot(pca,n=10,type="bar", cex=1.5, 
#'                      legend=TRUE,xlab="Components",ylab="Variance (\%)",
#'                      pc.col=c("light blue","grey"),...)}
#' \arguments{
#' \item{pca}{pca object which was created using the function `prcomp`}
#' \item{n}{maximal number of components to visualize, default: 10}
#' \item{type}{plotting type either "bar" or "scree", default: "bar"}
#' \item{cex}{character expansion for the legend and the screeplot plotting characters, default: 1.5}
#' \item{legend}{should the legend be displayed on top, default: TRUE}
#' \item{xlab}{label for the x-axis, default "Components"}
#' \item{ylab}{label for the y-axis, default "Variances(\%)"}
#' \item{pc.col}{colors for the PC variances, first individual, second color for the cumulative variance, default: c("light blue","grey")}
#' \item{\ldots}{additional arguments delegated to the standard plot function}
#' }
#' \value{NULL}
#' \examples{
#' data(iris)
#' par(mfrow=c(1,2))
#' pcai=prcomp(iris[,1:4],scale=TRUE)
#' athi$pca_plot(pcai)
#' athi$pca_plot(pcai,type="scree",legend=FALSE)
#' }
#' 

athi$pca_plot = function (pca,n=10,type="bar", cex=1.5, 
                     legend=TRUE,xlab="Components",ylab="Variance (%)",
                     pc.col=c("light blue","grey"),...) {
    if (n>ncol(pca$x)) {
        n=ncol(pca$x)
    }
    if (legend) {
        ylim=c(0,120)
    } else {
        ylim=c(0,105)
    }
    if (type=="bar") {
        barplot(summary(pca)$importance[3,1:n]*100,
                ylim=ylim,col='white',
                xlab=xlab,ylab=ylab,axes=FALSE,...)
    } else {
        plot(summary(pca)$importance[3,1:n]*100,type="b",
                ylim=ylim,cex.axis=1.2,lwd=2,cex=cex,
                xlab=xlab,ylab=ylab,axes=FALSE,
                pch=15,col=pc.col[2],...)
        points(summary(pca)$importance[2,1:n]*100,type="b",cex=cex,
                lwd=2,xlab="", pch=15,col=pc.col[1],...)

        axis(1,at=1:n,labels=paste("PC",1:n,sep=""))
    }
    axis(2,at=c(20,40,60,80,100),labels=c(20,40,60,80,100))
    if (type == "bar") {
        barplot(summary(pca)$importance[3,1:n]*100,add=TRUE,col=pc.col[2],axes=FALSE)
        barplot(summary(pca)$importance[2,1:n]*100,add=TRUE,col=pc.col[1],axes=FALSE)        
    }
    abline(h=5,lty=2,lwd=0.5)
    abline(h=10,lty=2,lwd=0.5)
    abline(h=90,lty=2,lwd=0.5)
    abline(h=95,lty=2,lwd=0.5)    
    abline(h=100,lty=1,lwd=0.5)    
    if (legend) {
        legend("topleft",c("Component","Cumulative"),col=pc.col,pch=15,cex=1.5,box.lwd=0,ncol=2)
    }
    box()
}

#' \name{athi$qr_plot}
#' \alias{athi$qr_plot}
#' \alias{athi_qr_plot}
#' \title{ Plot quantile regression models }
#' \description{
#'     This is a convinience method to plot quantile regression models and giving
#'     optional percentile intervals for a given range of predictions.
#'     The slope and the intercept for the different quantiles are returned as well if requested.
#' }
#' \usage{ athi_qr_plot(x,data,quantiles=c(0.05,0.1,0.5,0.9,0.95),pred=NULL,plot=TRUE,...)} 
#' \arguments{
#'   \item{x}{
#'     formula for two numerical variables
#'   }
#'   \item{data}{
#'     data frame containing the variables for the formula
#'   }
#'   \item{quantiles}{
#'     the requested quantiles, default: c(0.05,0.1,0.5,0.9,0.95)
#'   }
#'   \item{pred}{
#'     vector for prediction values, default: NULL
#'   }
#'   \item{plot}{
#'     should the data being plotted, default: TRUE
#'   }
#'   \item{\ldots}{other arguments which will be forwarded to the plot function}
#' }
#' \value{returns list with intercept and coefficients if requested (invisible)}
#' \examples{
#' data(iris) 
#' athi$qr_plot(Sepal.Width ~ Sepal.Length,data=iris[51:151,])
#' res=athi$qr_plot(Sepal.Width ~ Sepal.Length,data=iris[51:151,],pred=c(5,5.5,6,6.5,7,7.5,8))
#' res$centiles
#' res$coef
#' }
#' \seealso{
#'    \link[athi:athi-class]{athi-class}, \link[athi:athi_lm_plot]{athi$lm_plot}
#' }
#'


athi$qr_plot = function (x,data,quantiles=c(0.05,0.1,0.5,0.9,0.95),
                         pred=NULL,plot=TRUE,...) {
    if (!requireNamespace("quantreg")) {
        stop("Error: quantile regression needs package quantreg!")
    }
    seq=quantiles
    if (!is.null(pred[1])) {
        df=data.frame(y=pred)
        for (i in seq) {
            df=cbind(df,new=rep(0,nrow(df)))
            colnames(df)[ncol(df)]=paste("Perc",i,sep="_")
        }
    }
    multi_rqfit <- quantreg::rq(x, data = data, tau = seq)
    colors <- c("#ffe6e6", "#cca6a6", "#993333", "#cca6a6", "#ffe6e6")
    ltys=c(3,2,1,2,3)
    if (plot) {
        plot(x, data = data, pch = 16, ...)
        #print(multi_rqfit$coefficients)
        for (j in 1:ncol(multi_rqfit$coefficients)) {
            abline(coef(multi_rqfit)[, j], col = colors[j],lty=ltys[j],lwd=2)
        }
    }
    if (!is.null(pred[1])) {
        i = 1
        for (p in pred) {
            for (j in 1:ncol(multi_rqfit$coefficients)) {
                val=coef(multi_rqfit)[1, j]+p*coef(multi_rqfit)[2, j]
                df[i,j+1]=val
            }
            i=i+1
        }
    }
    if (!is.null(pred)[1]) {
        invisible(list(coef=coef(multi_rqfit),centiles=df))
    } else {
        invisible(list(coef=coef(multi_rqfit)))
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
#'   Perform data normalization on the current data sets such as z-score, feature scaling and so on.
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

#' \name{athi$sem}
#' \alias{athi$sem}
#' \alias{athi_sem}
#' \title{Calculate the standard error of the mean for a given numerical vector.}
#' \description{This function calculates the standard error of the mean, SEM = sd/sqrt(N).}
#' \usage{ athi_sem(x,na.rm=FALSE) }
#' \arguments{
#' \item{x}{a numerical vector}
#' \item{na.rm}{logical vecor indicating if missing values should be removed, default: FALSE}
#' }
#' \value{computed standard error of the mean}
#' \examples{
#' sem=athi$sem
#' sem(rnorm(50,mean=10,sd=3))
#' sem(rnorm(1000,mean=10,sd=3))
#' }
#' 

athi$sem <- function(x,na.rm=FALSE) {
    sd(x,na.rm=na.rm)/sqrt(length(x[!is.na(x)])) 
}

#' \name{athi$smartbind}
#' \alias{athi$smartbind}
#' \alias{athi_smartbind}
#' \title{Combine two data frames via rbind even with different column names.}
#' \description{This function combines two data frames or matrices even if they have different
#'   column names. In each data frame or matrix missing columns are first filled up
#'   with NA's. Thereafter data is fused using rbind. Column order is determined based 
#'   on the first given data.
#' }
#' \usage{ athi_smartbind(x,y) }
#' \arguments{
#'   \item{x}{data frame or matrix}
#'   \item{y}{data frame or matrix}
#' }
#' \value{data frame or matrix, depending on the given input where data from *x* and *y* are combined
#'   using rbind. Missing columns in either of the data frame are filled up with NA's.
#' }
#' \examples{
#' ir1=cbind(rn1=rnorm(nrow(iris)),iris)
#' ir2=cbind(iris,rn2=rnorm(nrow(iris),mean=10))
#' head(athi$smartbind(ir1,ir2))
#' tail(athi$smartbind(ir1,ir2))
#' }
#' \seealso{
#'   \link[athi:athi-class]{athi-class}
#' }
#' 

athi$smartbind <- function (x,y) {
    nxcols=setdiff(colnames(y),colnames(x))
    nycols=setdiff(colnames(x),colnames(y))
    for (c in nxcols) {
        x=cbind(x,ncol=rep(NA,nrow(x)))
        colnames(x)[ncol(x)]=c
    }
    for (c in nycols) {
        y=cbind(y,ncol=rep(NA,nrow(y)))
        colnames(y)[ncol(y)]=c
    }
    y=y[,colnames(x)]
    x=rbind(x,y)
    return(x)
}


#' \name{athi$textplot}
#' \alias{athi$textplot}
#' \alias{athi_textplot}
#' \title{Write the data for a data frame or matrix into a plot.}
#' \description{This function can be used as a workaround to display data for small
#'   data frames or matrices with let's say 3-8 rows or columns into a plot. 
#' }
#' \usage{ athi_textplot(x,cex=1,caption=NULL,...) }
#' \arguments{
#' \item{x}{data frame or matrix}
#' \item{cex}{character expansion, default: 1}
#' \item{caption}{optional caption shown below of the table, default: NULL}
#' \item{\ldots}{remaining arguments delegated to the plot function}
#' }
#' \value{NULL}
#' \examples{
#' par(mai=rep(0.1,4))
#' athi$textplot(head(swiss),caption="Table 1: Swiss data first six lines")
#' }
#' \seealso{
#'   \link[athi:athi-class]{athi-class}
#' }
#' 

athi$textplot <- function (x,cex=1,caption=NULL,...) {
    xlim=c(-0.5,ncol(x)+0.5)
    ylim=c(0,nrow(x)+1.4)
    if (!is.null(caption)) {
        ylim[1]=-1
    }
    plot(1,type="n",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",...)
    if (!is.null(caption)) {
        text(mean(xlim),-0.6,caption,cex=cex*1.2)
    }
    abline(h=nrow(x)+1.5,lwd=2)
    abline(h=nrow(x)+0.6,lwd=2)
    abline(h=0.3,lwd=2)    
    for (i in 1:nrow(x)) {
        text(-0.5,nrow(x)+1-i,rownames(x)[i],cex=cex,adj=0)
        for (j in 1:ncol(x)) {
            if (i == 1) {
                text(j,nrow(x)+1,colnames(x)[j],cex=cex)
            }
            text(j,nrow(x)+1-i,x[i,j],cex=cex)
        }
    }
}

#' \name{athi$untab}
#' \alias{athi$untab}
#' \alias{athi_untab}
#' \title{Convert a contingency table to a data frame one item per row.}
#' \description{This function takes a contingency table and expands it two a data frame
#'   where every level combinations is created according to the number of entries in the
#'   contingency table. You can reverse the process using the table command. The procedure is useful for
#'   instance for performing sample bootstrapping.
#' }
#' \usage{ athi_untab(x) }
#' \arguments{
#' \item{x}{contingency table or a matrix}
#' }
#' \value{data frame with two columns, column names are either the dimnames of the given table or
#'             generic columnames like 'rowvar' and 'colvar'.}
#' \examples{
#' M=matrix(c(4,3,1,2),ncol=2,dimnames=list(
#'         size=c('small','tall'),
#'         smoking=c('no','yes')))
#' M
#' N=athi$untab(M)
#' head(N)
#' summary(N)
#' table(N[,1],N[,2])
#' }
#' \seealso{
#'   \link[athi:athi-class]{athi-class}
#' }
#' 

athi$untab <- function (x) {
    tab=x
    nms=names(dimnames(tab))
    if (any(is.null(nms)))
    nms=c('rowvar','colvar')
    df=data.frame(a=c(),b=c())
    for (i in 1:nrow(tab)) {
        for (j in 1:ncol(tab)) {
            if (tab[i,j]> 0) {
                for (k in 1:tab[i,j]) {
                    if (nrow(df)==0) {
                        df=data.frame(vrow=rownames(tab)[i],vcol=colnames(tab)[j])
                    } else {
                        df=rbind(df,data.frame(vrow=rownames(tab)[i],vcol=colnames(tab)[j]))
                    }
                }
            }
        }
    }   
    df[,1]=as.factor(df[,1])
    df[,2]=as.factor(df[,2])
    colnames(df)=nms
    return(df)
}

#' \name{athi$venn}
#' \alias{athi$venn}
#' \alias{athi_venn}
#' \title{Plot Venn diagram for logical relations between two or three sets.}
#' \description{This function combines two data frames or matrices even if they have different
#'   column names. In each data frame or matrix missing columns are first filled up
#'   with NA's. Thereafter data is fused using rbind. Column order is determined based 
#'   on the first given data.
#' }
#' \usage{ athi_venn(x,y=NULL,z=NULL,vars=NULL,col=c("#cc888899","#8888cc99","#88cc8899"),cex=1.6,...) }
#' \arguments{
#' \item{x}{data frame or matrix or vector, in the latter case y and for 3 sets as well z must be given}
#' \item{y}{vector if _x_ is vector, default: NULL}
#' \item{z}{vector if _x_ and _y_ are vectors, default: NULL}
#' \item{vars}{variable names to display if x, y and z are vectors, default: NULL}
#' \item{col}{background colors for the circles, default: c("#cc888899","#8888cc99","#88cc8899")}
#' \item{cex}{character expansion for the text characters, default: 1.6}
#' \item{\ldots}{argument delegated to the plot function}
#' }
#' \value{NULL}
#' \examples{
#' X=matrix(rnorm(2700),ncol=3)
#' colnames(X)=c("A","B","C")
#' X=X>0.4
#' Y=X[,1:2]
#' Y=Y>0.7
#' par(mfrow=c(2,2),mai=rep(0.1,4),pty='s')
#' athi$venn(X)
#' athi$venn(Y)
#' athi$venn(x=LETTERS[1:9],y=LETTERS[3:6],z=LETTERS[4:12],vars=c('Xvar','Yvar','Zvar'))
#' athi$venn(x=LETTERS[1:9],y=LETTERS[3:6],vars=c('Xvar','Yvar'))
#' }
#' \seealso{
#'   \link[athi:athi-class]{athi-class}
#' }
#'  
athi$venn = function (x,y=NULL,z=NULL,vars=NULL,col=c("#cc888899","#8888cc99","#88cc8899"),cex=1.6,...) {
    circle = function (x,y, radius=1,length=100) {
        theta = seq(0, 2 * pi, length = 100) 
        return(list(x=radius*cos(theta)+x,
                    y=radius*sin(theta)+y))
    }
    venn2D = function (x,col=c("#cc888899","#8888cc99"),cex=1.6,...) {
        if (!is.data.frame(x) & !is.matrix(x)) {
            stop("Error: Not a two column matrix or data frame!")
        }
        if (ncol(x) != 2) {
            stop("Error: Not a two column matrix or data frame!")   
        }
        # reset to useful values, slightly smaller than the defaults:
        # defaults: mai=c(1.02, 0.82, 0.82, 0.42)
        #opar=par(mai=c(1, 0.8, 0.8, 0.4),pty='s')
        # compute circle size
        circ.cex=60*par()$fin[1]/9
        
        plot(c(1,2),c(1,1),xlim=c(0.5,2.5),ylim=c(0.5,2.5),
             pch=19,cex=circ.cex,axes=FALSE,type="n",
             xlab="",ylab="",
             col=col,...)
        polygon(circle(1.2,1.5,radius=0.65),col=col[1],border=col[1])
        polygon(circle(1.8,1.5,radius=0.65),col=col[2],border=col[2])
        text(1.1,2.3,colnames(x)[1],cex=cex)
        text(1.9,2.3,colnames(x)[2],cex=cex)
        # the changes
        if (class(x[,1]) == "logical") {
            is=length(which(x[,1] & x[,2]))
            ls=length(which(x[,1] & !x[,2]))
            rs=length(which(!x[,1] & x[,2]))
            os=length(which(!x[,1] & !x[,2]))
        } else {
            xv=x[,1]
            xv=xv[xv!=""]
            yv=x[,2]
            yv=yv[yv!=""]
            is=length(intersect(xv,yv))
            ls=length(setdiff(xv,yv))
            rs=length(setdiff(yv,xv))
            os=""
        }   
        text(1.5,1.5,is,cex=cex)
        text(0.9,1.5,ls,cex=cex)
        text(2.1,1.5,rs,cex=cex)
        text(1.5,0.7,os,cex=cex)
        #par(opar)
    }                                                   
    if (class(y)[1] != "NULL" & class(z)[1]!="NULL") {
        M=matrix('',ncol=3,nrow=max(c(length(x),length(y),length(z))))
        M[1:length(x),1]=x
        M[1:length(y),2]=y
        M[1:length(z),3]=z        
        colnames(M)=c('x','y','z')
        if (class(vars[1])!="NULL") {
            colnames(M)=vars
        }
        athi$venn(M,col=col,cex=cex,...)
    } else if (class(y)[1] != "NULL") {
        M=matrix('',ncol=2,nrow=max(c(length(x),length(y))))
        M[1:length(x),1]=x
        M[1:length(y),2]=y
        colnames(M)=c('x','y')
        if (class(vars[1])!="NULL") {
            colnames(M)=vars
        }
        athi$venn(M,col=col,cex=cex,...)
    } else if (!is.data.frame(x) & !is.matrix(x)) {
        stop("Error: Not a matrix or data frame!")
    } else if (ncol(x) == 2) {
        venn2D(x,col=col[1:2],cex=cex,...)
    } else if (ncol(x) != 3) {
        stop("Error: Only two or three column matrix or data frame is accepted!")  
    } else if (!class(x[,1]) == "logical") {    
        rnames=unique(c(x[,1],x[,2],x[,3]))
        rnames=rnames[which(rnames!="")]
        M=matrix(FALSE,ncol=3,nrow=length(rnames))
        rownames(M)=rnames
        colnames(M)=colnames(x)
        for (i in 1:3) {
            idx=which(rnames%in%x[,i])
            M[idx,i]=TRUE
        }   
        athi$venn(M,col=col,cex=cex,...)
    } else {  
        #opar=par(mai=c(0.5, 0.4, 0.4, 0.2),pty='s')
        circ.cex=70*par()$fin[1]/9
        plot(c(3.5,5.5,4.5),c(5.5,5.5,3.5),xlim=c(0,9),ylim=c(0,9),
             pch=19,cex=circ.cex,axes=FALSE,asp=1,
             xlab="",ylab="", col=col,type="n",...)
        polygon(circle(3.5,5.5,radius=2.3),col=col[1],border=col[1])
        polygon(circle(5.5,5.5,radius=2.3),col=col[2],border=col[2])
        polygon(circle(4.5,3.5,radius=2.3),col=col[3],border=col[3])
        
        text(0.5,7,colnames(x)[1],cex=cex)
        text(8.5,7,colnames(x)[2],cex=cex)
        text(4.5,0.25,colnames(x)[3],cex=cex)
        is=length(which(x[,1] & x[,2] & x[,3]))
        ls=length(which(x[,1] & !x[,2] & !x[,3]))
        rs=length(which(!x[,1] & x[,2] & !x[,3]))
        bs=length(which(!x[,1] & !x[,2] & x[,3]))        
        os=length(which(!x[,1] & !x[,2] & !x[,3]))
        xys=length(which(x[,1] & x[,2] & !x[,3]))
        xzs=length(which(x[,1] & !x[,2] & x[,3]))        
        yzs=length(which(!x[,1] & x[,2] & x[,3]))                
        text(4.5,4.8,is,cex=cex*0.8)
        if (os>0) {
            text(4.5,8.5,os,cex=cex*0.8)
        }
        text(2.2,6,ls,cex=cex*0.8)
        text(6.8,6,rs,cex=cex*0.8)
        text(4.5,2.1,bs,cex=cex*0.8)
        text(4.5,6.6,xys,cex=cex*0.8)    
        text(2.9,4,xzs,cex=cex*0.8)        
        text(6,4,yzs,cex=cex*0.8)            
        #par(opar)
    }   
}

#' \name{athi$wilcoxR}
#' \alias{athi$wilcoxR}
#' \alias{athi_wilcoxR}
#' \title{Calculate the effect size for a wilcox test (Rosenthal 1991).}
#' \description{This function calculates the effect size for a numerical vector with non-normally distributed variables
#'  and a categorical vector with two levels.
#' }
#' \usage{ athi_wilcoxR(x,y=NULL,n=NULL) }
#' \arguments{
#' \item{x}{either a wilcox test object or a vector with numerical values}
#' \item{y}{if _x_ is numerical vector either a vector with numerical values or a vector with cvategorical data having the same length as x}
#' \item{n}{numnber of samples, required if _x_ is a wilcox.test object}
#' }
#' \value{numerical value for effect size r having the same interpretation rules a Pearson's r}
#' \examples{
#' set.seed(123)
#' rn1=rnorm(100,mean=10,sd=1)
#' rn2=rnorm(80,mean=11,sd=1)
#' rn2=c(rn2,rnorm(20,mean=12,sd=2)) # bimodal distribution
#' mean(rn1)
#' mean(rn2)
#' athi$wilcoxR(rn1,rn2) # two numerical vectors
#' wt=wilcox.test(rn1,rn2,exact=FALSE) 
#' athi$wilcoxR(wt,n=200) # a test object
#' cat = as.factor(c(rep('A',100),rep('B',100)))
#' athi$wilcoxR(x=c(rn1,rn2),y=cat) # num ~ cat
#' }
#' 
#' \seealso{
#'   \link[athi:athi-class]{athi-class},\link[athi:athi_cohensD]{athi$cohensD}, 
#'   \link[athi:athi_cohensW]{athi$cohensW}, \link[athi:athi_eta_squared]{athi$eta_squared},
#'   \link[athi:athi_epsilon_squared]{athi$epsilon_squared}
#' }

athi$wilcoxR <- function (x,y=NULL,n=NULL) {
    if (class(y)[1] %in% c("numeric","integer")) {
        wt=wilcox.test(x,y,exact=FALSE)
        n=length(x[which(!is.na(x))]) +  length(x[which(!is.na(x))])
        return(athi$wilcoxR(wt,n=n))
    }  else if (class(y)[1] %in% c("factor")) {
        wt=wilcox.test(x~y,exact=FALSE)
        n=length(x[which(!is.na(x) & !is.na(y))])        
        return(athi$wilcoxR(wt,n=n))
    } else {
        z = qnorm(x$p.value/2)
        return(abs(z/sqrt(n)))
    }
}

athi_assoc_plot = athi$assoc_plot
athi_bootstrap = athi$bootstrap
athi_box_plot = athi$box_plot
athi_cdist = athi$cdist
athi_chr2ord = athi$chr2ord
athi_cihist  = athi$cihist
athi_cohensD = athi$cohensD
athi_cohensW = athi$cohensW
athi_corr = athi$corr
athi_cor_plot = athi$cor_plot
athi_corr_plot = athi$corr_plot
athi_cv = athi$cv
athi_df2md = athi$df2md
athi_drop_na = athi$drop_na
athi_epsilon_squared = athi$epsilon_squared
athi_eta_squared = athi$eta_squared
athi_fmt = athi$fmt
athi_impute = athi$impute
athi_input = athi$input
athi_introNAs = athi$introNAs
athi_kroki = athi$kroki
athi_lm_plot = athi$lm_plot
athi_mi = athi$mi
athi_mds_plot = athi$mds_plot
athi_pastel = athi$pastel
athi_pca_biplot = athi$pca_biplot
athi_pca_oncor = athi$pca_oncor
athi_pca_pairs = athi$pca_pairs
athi_pca_plot = athi$pca_plot
athi_qr_plot = athi$qr_plot
athi_ref_score = athi$ref_score
athi_ref_table = athi$ref_table
athi_report_pvalue = athi$report_pvalue
athi_norm = athi$norm
athi_randomize = athi$randomize
athi_sem = athi$sem
athi_smartbind = athi$smartbind
athi_textplot = athi$textplot
athi_untab = athi$untab
athi_venn = athi$venn
athi_wilcoxR = athi$wilcoxR
