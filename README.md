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
signal <- read_csv_signals(sig_file)

out_mp <- mp_omp_execute(
  signal = signal,
  mode = "mp",          # use "omp" for Orthogonal Matching Pursuit
  topk = 10000,
  n_nonzero_coefs = 50,
  verbose = TRUE
)

plot(out, channel = 1, freq_divide = 4)
```

If `dictionary = NULL`, an XML Gabor dictionary specification is generated internally.

The below time–frequency map shows the energy distribution of the selected atoms. 
White crosses indicate the centers of individual time–frequency blobs, i.e. 
the atoms' central time and central frequency coordinates. The panels below 
show the original signal and its reconstruction; in this example, the reconstruction 
explains 96.9% of the signal energy.

<p align="center">
  <img src="man/figures/mp.png" width="800">
</p>

### EMPI

EMPI must first be installed using the `empi_install()` function. The decomposition 
can then be performed using `empi_execute()` and visualized using `plot()`.

```r
sig_file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
signal <- read_csv_signals(sig_file)

out_empi <- empi_execute(
  signal = signal
)

plot(out_empi, channel = 1, freq_divide = 4)
```
This example illustrates an EMPI-based decomposition and its time–frequency 
representation. The map reveals several localized components distributed across 
time and frequency, while the reconstructed signal closely follows the original 
waveform. In this case, the selected atoms explains 98.96% of the signal energy.

<p align="center">
  <img src="man/figures/empi.png" width="800">
</p>

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
[1] 1 4

out$relative_residual_energy
[1] 1.000000e+00 2.012529e-01 4.867641e-32
```

In the example above, OMP correctly identifies atoms 1 and 4, corresponding to 
the 5 Hz sine and 10 Hz cosine components used to construct the signal. After 
two iterations, the residual energy is effectively zero.

## Educational OMP implementation

For small illustrative examples, `omp_reference()` provides a transparent 
educational implementation that follows the mathematical formulation of OMP 
explicitly, including least-squares coefficient updates and residual orthogonality 
checks.

```r
ref <- omp_reference(
  dictionary = dictionary,
  signal = signal,
  n_nonzero_coefs = 2
)

ref$selected_atoms
[1] 1 4

ref$coefficients_original_dict
[1] 1.0 0.0 0.0 0.5

ref$normalized_reconstruction_error_original_dict
[1] 6.95e-16

ref$orthogonality
[[1]]
-5.03e-15

[[2]]
-5.30e-15  7.07e-15
```

The reference implementation recovers the exact coefficients of the original 
dictionary, while the near-zero reconstruction error and orthogonality values 
confirm the expected OMP properties.


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
