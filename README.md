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
* `athi_df2md` - print a matrix or data frame as a Markdown table
* `athi$impute` - impute missing values using rpart or knn methods
* `athi$introNAs` - introduce missing values
* `athi$lm_plot`  - plot a linear  model  with  confidence  intervals
* `athi$mds_plot` - plot a multidimensional scaling analysis
* `athi$norm` - normalize data
* `athi$randomize` - randomize column data within matrix or data frame
* `athi$ref_score` - WHO  reference score for the given age, sex and type
* `athi$ref_table` - reference tables for WHO for the given sex and measure type

## Install

The package can be directly  installed from the Github  repository start R and
install the package with this command:

```{r}
> install.packages("https://github.com/mittelmark/athi/archive/refs/tags/0.3.0.tar.gz",repos=NULL)
```
## Author and Copyright

Author: Detlef Groth, University of Potsdam, Germany

License: MIT License see the file [LICENSE](LICENSE) for details.

## Bug reporting

In case of bugs and suggestions, use the [issues](https://github.com/mittelmark/athi/issues) link on top.
