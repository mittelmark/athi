# athi  R package

A R package developed for the 6. International  Summer School "Human Biology and
Public Health - Data Analysis and  Statistics" of the University of Potsdam in
Gülpe in the North-East of Brandenburg.

The package contains a few functions required for reliable data analysis which
are not part or only in more basic format in standard R.

The following  methods are provided within the  athi-environment  from version
0.4.0:

*  `athi$assoc_plot`  - assocplot with statistics  values and Pearson residual
  shading (plot)
* `athi$box_plot` - boxplot with statistics values (plot)
* `athi$bootstrap` - resampling for the given data set and function (stats)
* `athi$cdist` - correlation distances (stats)
* `athi$chr2ord` - map characters or factors to numbers with comfort (data)
* `athi$cihist` - confidence interval for histogram density plots (plot)
* `athi$cohensD` - effect size for difference between two means (stats)
* `athi$cohensW` - effect size for categorical data (stats)
* `athi$corr` - pairwise correlations and their statistics (stats)
* `athi$cor_plot` - extended xy-plot with statistics (plot)
* `athi$df2md` - print a matrix or data frame as a Markdown table (document)
* `athi$drop_na` - drop rows if the given columns  contain NAs in this row for
  certain columns (data)
* `athi$epsilon_squared` - effect size measure for Kruskal test (stats)
* `athi$eta_squared` - effect size measure for ANOVA (stats)
* `athi$fmt` - formatted string output (data)
* `athi$impute` - impute missing values using rpart or knn methods (data)
* `athi$input` - readline replacement for scripts (scripts)
* `athi$introNAs` - introduce missing values (data)
* `athi$kroki` - draw diagrams and flowcharts using kroki webservice (plot)
* `athi$lm_plot`  - plot a linear  model  with  confidence  intervals (plot
* `athi$mds_plot` - plot a multidimensional scaling analysis (plot)
* `athi$mi` - determine mutual information (stats)
* `athi$norm` - normalize data (data)
* `athi$pastel` - create up to 20 pastel colors (plot)
* `athi$pca_biplot` - improved biplot for pca objects (plot)
* `athi$pca_oncor` - perform a PCA on a square (correlation) matrix (stats)
* `athi$pca_pairs` - improved pairs plot for pca objects (plot)
* `athi$pca_plot` - improved screeplot for pca objects (plot)
* `athi$pcor` - measuring partial correlation (stats)
* `athi$pcor.test` - test for significance of partial correlation (stats)
* `athi$randomize` - randomize column data within matrix or data frame (data)
* `athi$ref_score` - WHO  reference score for the given age, sex and type (data)
* `athi$ref_table`  - reference  tables for WHO for the given sex and measure
  type (data)
* `athi$report_pvalue` - report p-values within documents using default alpha
  thresholds (document)
* `athi$sem` - standard error of the mean (stats)
* `athi$smartbind` - combine two data frame even if the have different column names (data)}
* `athi$textplot` - write the data for a data frame or matrix into a plot (plot)
* `athi$untab` - expand a contingency table to a data frame one item per row (data)
* `athi$venn` - Venn diagram for logical relations between two and three sets (plot)
* `athi$wilcoxR` - effect size r, for a wilcox test object (stats)

## Install

The package can be directly  installed from the Github  repository like this:
Start R and install the package with the following command:

```r
install.packages(
    "https://github.com/mittelmark/athi/releases/download/0.4.0/athi_0.4.0.tar.gz",
    repos=NULL)
```
  
To install the latest  development  version directly from the Github repository
you need the R package `remotes` to be installed, if you have this package you can
do the following:

```{r}
library(remotes)
remotes::install_github("https://github.com/mittelmark/athi")
```

You should do this if you need  functions  from the  listing  above  which are
currently only on Github, but not in the latest release.

## Documentation

The package comes with the usual documentation and vignettes.

To see the main entry page for the documentation use the library help function
like this:

```{.r}
library(athi)
library(help="athi")
```

To browse the vignette(s) use the vignette command like this:

```{.r}
library(athi)

vignette("athi-functions",package="athi")
```

## Author and Copyright

Author: Detlef Groth, University of Potsdam, Germany

License: MIT License see the file [LICENSE](LICENSE) for details.

## Bug reporting

In case of bugs and suggestions, use the [issues](https://github.com/mittelmark/athi/issues) link on top.
