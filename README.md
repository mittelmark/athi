# athi  R package

A R package developed for the 6. International  Summer School "Human Biology and
Public Health - Data Analysis and  Statistics" of the University of Potsdam in
Gülpe in the North-East of Brandenburg.

The package contains a few functions required for reliable data analysis which
are not part or only in more basic format in standard R.

The following  methods are provided within the  athi-environment  from version
0.3.0:

* `athi$cdis` - correlation distances
* `athi$cohensD` - effect size for difference between two means
* `athi$cohensW` - effect size for categorical data
* `athi$corr` - pairwise correlations and their statistics
* `athi$df2md` - print a matrix or data frame as a Markdown table
* `athi$impute` - impute missing values using rpart or knn methods
* `athi$introNAs` - introduce missing values
* `athi$lm_plot`  - plot a linear  model  with  confidence  intervals
* `athi$mds_plot` - plot a multidimensional scaling analysis
* `athi$norm` - normalize data
* `athi$randomize` - randomize column data within matrix or data frame
* `athi$ref_score` - WHO  reference score for the given age, sex and type
* `athi$ref_table` - reference tables for WHO for the given sex and measure type

## Install

The package can be directly  installed from the Github  repository like this:
Start R and install the package with the following command:

```r
install.packages(
    "https://github.com/mittelmark/athi/archive/refs/tags/0.3.0.tar.gz",
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

## Author and Copyright

Author: Detlef Groth, University of Potsdam, Germany

License: MIT License see the file [LICENSE](LICENSE) for details.

## Bug reporting

In case of bugs and suggestions, use the [issues](https://github.com/mittelmark/athi/issues) link on top.
