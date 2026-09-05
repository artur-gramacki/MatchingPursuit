# MatchingPursuit: An R Framework for Sparse Time-Series Decomposition Using Matching Pursuit and Orthogonal Matching Pursuit

<!-- badges: start -->
[![CRAN status](http://www.r-pkg.org/badges/version/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/MatchingPursuit)](https://CRAN.R-project.org/package=MatchingPursuit)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/MatchingPursuit)](https://cran.r-project.org/package=MatchingPursuit)
<!-- badges: end -->

## Purpose

Sparse signal decomposition framework for one- and multi-channel biomedical and 
general time-series data using the **Matching Pursuit** and **Orthogonal Matching Pursuit** 
algorithms.


Supported features:

- Sparse decomposition using arbitrary user-defined dictionaries
- Native R implementations of:
    - Matching Pursuit (**MP-R**)
    - Orthogonal Matching Pursuit (**OMP-R**)
- An educational and reference implementation of OMP through `omp_reference()`
- Specialized Gabor dictionary support for time-frequency decomposition
- Optional high-performance Gabor-based Matching Pursuit using the external
  **Enhanced Matching Pursuit Implementation (EMPI)** backend
- Support for biomedical signal formats:
    - EDF / EDF+ files
    - WFDB records
    - three standard EEG montages (bipolar, referential, and average-reference montages)
- Plotting time-frequency maps for Gabor-based MP, OMP, and EMPI results
- Pre-filtering signals using notch, low-pass, high-pass, band-pass, and band-stop filters

Note:

The terms **MP-R** and **OMP-R**, with the suffix **R**, refer to the corresponding 
native R backends rather than to the algorithms themselves. The terms **MP** and 
**OMP**, without the suffix **R**, refer to the corresponding algorithms rather 
than to a specific implementation.

## Installation

You can install the released version from
[CRAN](https://CRAN.R-project.org) with:

```r
install.packages("MatchingPursuit")
```

## Quick start

### MP-R and OMP-R: Gabor-based decomposition

For Gabor-based decomposition, `mp_omp_execute()` provides the high-level interface. 
It prepares the Gabor dictionary, selects candidate atoms, performs MP or OMP 
decomposition, combines the numerical results with Gabor metadata, and returns an 
object of class `"mp"`.

```r
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
xml_file <- system.file("extdata", "sample1.xml", package = "MatchingPursuit")

signal <- read_csv_signals(sig_file, col_names = "ch1")

out <- mp_omp_execute(
  signal = signal,
  mode = "omp",                # use "mp" for Matching Pursuit
  dictionary = xml_file,
  topk = 5000,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

plot(out, channel = 1, freq_divide = 4)
```

If `dictionary = NULL`, an XML Gabor dictionary specification is generated internally.

### EMPI

EMPI must first be installed using the `empi_install()` function. The decomposition 
can then be performed using `empi_execute()` and visualized using `plot()`.

```r
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal <- read_csv_signals(sig_file)

out <- empi_execute(
  signal = signal
)

plot(out, channel = 1, freq_divide = 4)
```

### Decomposition with a custom dictionary

The native R implementations can operate directly on arbitrary user-defined 
dictionaries. Dictionary atoms are supplied as columns of a numeric matrix and 
are normalized internally to unit L2 norm.

```r
N <- 256
t <- seq(0, 1, length.out = N)

dictionary <- cbind(
  sin(2 * pi * 5 * t),
  cos(2 * pi * 5 * t),
  sin(2 * pi * 10 * t),
  cos(2 * pi * 10 * t)
)

signal <- sin(2 * pi * 5 * t) + 0.5 * cos(2 * pi * 10 * t)

out <- omp_core(
  dictionary = dictionary,
  signal = signal,
  n_nonzero_coefs = 2
)

out$support
out$coefs
out$relative_residual_energy
```

For small illustrative examples, `omp_reference()` provides a transparent 
educational implementation that follows the mathematical formulation of OMP 
explicitly, including least-squares coefficient updates and residual orthogonality 
checks.

## Package architecture

The package provides three main decomposition routes.

| Route | Main functions | Purpose | Recommended use |
|:--|:--|:--|:--|
| **General matrix-based MP/OMP** | `mp_core()`, `omp_core()` | Sparse decomposition with arbitrary matrix dictionaries | General sparse decomposition and experimentation |
| **Native R Gabor-based MP/OMP** | `mp_omp_execute()` | High-level Gabor-based MP or OMP decomposition | Time-frequency decomposition in R |
| **External EMPI-based MP** | `empi_execute()` | Optimized Gabor-based Matching Pursuit | Large-scale time-frequency decomposition |

Notes:

1. `mp_core()` and `omp_core()` are general-purpose low-level sparse solvers. 
They operate on arbitrary numeric matrix dictionaries and are independent of the 
Gabor-specific workflow.

2. `mp_omp_execute()` provides the high-level native R interface for Gabor-based 
decomposition. If no XML dictionary specification is supplied, `generate_xml_dict()` 
creates one internally. The specification is processed by `read_gabor_dict()`, 
after which `topk_gabor_atoms()` selects channel-specific candidate Gabor atoms. 
The resulting atom matrices are passed to `mp_core()` or `omp_core()`, and the 
numerical results are combined with Gabor metadata to construct an object of 
class `"mp"`.

3. EMPI is an optional external high-performance C++ backend specialized in 
Gabor-based Matching Pursuit. It supports optimized CPU execution and GPU 
acceleration.

4. `omp_reference()` is a straightforward educational and reference 
implementation of OMP intended for small illustrative examples. It explicitly 
solves the least-squares problem using the normal-equation formula and is not 
intended for large-scale or numerically demanding computations.

## Typical workflows

The diagram below summarizes the three available decomposition workflows and
their relationship within the package.

<p align="center">
  <img src="man/figures/main_workflows.png" width="600">
</p>

The `mp_omp_execute()` function provides a high-level interface for Gabor-based MP 
and OMP decomposition. It combines the individual steps implemented by 
`generate_xml_dict()`, `read_gabor_dict()`, `topk_gabor_atoms()`, and either 
`mp_core()` or `omp_core()` into a single workflow.

<p align="center">
  <img src="man/figures/flow_of_mp_omp_execute.png" width="520">
</p>


Signal input for the high-level workflows can be imported using `read_csv_signals()`, 
`read_edf_signals()`, or `read_wfdb_signals()`.

`tf_map()` is available for decomposition results that contain time-frequency metadata, 
such as Gabor-based MP/OMP and EMPI results.

## Documentation

The package documentation includes:

- Introduction vignette
- Reference manual

## Supported input formats

The package supports generic multichannel time-series together with commonly 
used biomedical formats:

- CSV (generic signals)
- EDF / EDF+ (EEG, ECG)
- WFDB (physiological records)

## License

GPL-3
