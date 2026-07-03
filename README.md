# MatchingPursuit

<!-- badges: start -->
[![CRAN status](http://www.r-pkg.org/badges/version/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)
<!-- badges: end -->

## Purpose

Sparse signal decomposition framework for one- and multi-channel biomedical and general time series data using the **Matching Pursuit (MP)** and **Orthogonal Matching Pursuit (OMP)** algorithms.

Supported features:

- Matching Pursuit 
  - optimized C++ backend (**EMPI**)
  - reference R implementation (**MP**)
- Orthogonal Matching Pursuit (**OMP**) for improved sparse reconstruction accuracy
- Gabor dictionary support via XML-based atom definitions
- Support for biomedical signal formats:
  - EDF / EDF+ file support
  - WFDB records support
- Time-frequency analysis and visualization tools

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MatchingPursuit")
```

## Quick start

### MP and OMP
The simplest workflow consists of four steps:

1. Read a signal.
2. Read a dictionary definition.
3. Select the most relevant atoms.
4. Perform MP or OMP decomposition.

or, equivalently, execute the complete pipeline using a single function `mp_omp_run_pipeline()`. 
After executing you can plot time-frequency map.

```
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
xml_file <- system.file("extdata", "sample1_dict.xml", package = "MatchingPursuit")

out <- mp_omp_run_pipeline(
  mode = "mp",                # or "omp" for Orthogonal Matching Pursuit
  sig_file = sig_file,
  col_names_in_csv = FALSE,
  xml_file = xml_file,
  topk = 5000,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

plot(out, channel = 1)
```
### EMPI
The EMPI tool must be first installed via `empi_install()` function. Then, 
the simplest way is to run the `empi_execute()` function. After the decomposition, 
the result can be visualized using `plot()` function.

```
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
sig <- read_csv_signals(sig_file)

out <- empi_execute(
  signal = sig
)

plot(out, channel = 1)
```

## Package architecture

The package contains three computational backends designed for different purposes.


| Backend                               | Main functions   | Purpose                                          | Recommended use                |
|:--------------------------------------|:-----------------|:-------------------------------------------------|:-------------------------------|
| **Optimized C++ backend (EMPI)**      | `empi_execute()` | Large datasets and production analyses           | Optimized implementation       |
| **Matching Pursuit (MP)**             | `mp_core()`      | Learning, debugging and experimentation          | Reference implementation       |
| **Orthogonal Matching Pursuit (OMP)** | `omp_core()`     | Greedy sparse solver with orthogonal projections | Higher reconstruction accuracy |

Notes:

1. The pure R implementation is intentionally included as a readable reference implementation. It 
follows the classical Matching Pursuit algorithm but is significantly slower than the optimized 
C++ implementation.

2. EMPI is a black-box optimized C++ implementation of Matching Pursuit.
It performs dictionary construction, atom selection, and reconstruction internally.

3. Orthogonal Matching Pursuit extends the classical Matching Pursuit algorithm by recomputing 
coefficients using orthogonal projections, often yielding more accurate sparse approximations.

4. Unlike the MP/OMP workflow, which explicitly constructs a dictionary
(`read_dict()`) and selects candidate atoms (`topk_atoms()`), EMPI performs
these steps internally as part of a single optimized C++ pipeline.

5. As a result:

- MP/OMP provides full transparency and control over each stage
- EMPI provides significantly higher performance with minimal user overhead

## Typical workflow
 
``` 
                  START
                    |
        ------------|------------
        |                       |
 MP / OMP workflow         EMPI workflow
        |                       | 
 read_csv_signals()      read_csv_signals()
        │                       │
  read_dict()                   |             
        │                       │
  topk_atoms()                  │
        │                       │
 mp_omp_execute()         empi_execute()
        |                       |
        ------------------------
                    |
            plot() / tf_map()
``` 

## Documentation

The package documentation includes:

 - Introduction vignette
 - Reference manual
 - Examples
 - Time-frequency visualization

## Supported input formats

The package supports generic multichannel time-series together with commonly used biomedical formats:

- CSV (generic signals)
- EDF / EDF+ (EEG, ECG)
- WFDB (physiological records)

## License

GPL-3

