# MatchingPursuit

<!-- badges: start -->
[![CRAN status](http://www.r-pkg.org/badges/version/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)

<!-- badges: end -->

R package for processing time series data using the **Matching Pursuit** (**MP**) algorithm, 
a greedy signal decomposition technique that represents complex signals as a linear combination 
of simpler functions (called atoms) selected from a redundant dictionary. Support for the 
**Orthogonal Matching Pursuit** (**OMP**) variant of the classical MP algorithm is also provided.

In addition to generic time-series data, the package supports direct loading of
data stored in **EDF/EDF(+)** and **WFDB** (WaveForm DataBase) formats.
These formats are widely used for physiological signals such as EEG and ECG recordings.
Support for EDF/EDF(+) and WFDB import facilitates the analysis of biomedical signals.

## Installation

You can install the released version of motif from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MatchingPursuit")
```

## Learning more

To get started, first read 

[Introduction to MatchingPursuit package](https://cran.r-project.org/web/packages/MatchingPursuit/vignettes/MatchingPursuit.html)

