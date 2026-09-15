# MatchingPursuit 1.3.0

* Simplified the native MP/OMP core interfaces. `mp_core()` and `omp_core()`
  now accept numeric vector, matrix, or data frame dictionaries directly and 
  no longer accept `topk` objects. Dictionary atoms are internally normalized 
  to unit L2 norm before decomposition, making atom selection invariant to
  non-zero scaling of dictionary columns.
  
* Updated the high-level Gabor-based MP/OMP workflow in `mp_omp_execute()`,
  which now internally calls `read_gabor_dict()` and `topk_gabor_atoms()`.
  The `dictionary` argument has changed: it now accepts either `NULL` or a path
  to an XML dictionary file, instead of a precomputed `topk` object. When
  `dictionary = NULL`, the XML dictionary is generated internally.
  
* Added the `full_atoms_in_signal` parameter to `mp_omp_execute()`, which
  controls whether Gabor atoms must be fully contained within the signal
  boundaries. Allowing atoms to extend beyond the signal boundaries can
  substantially improve decomposition quality, particularly near the
  beginning and end of the signal.  

* Renamed `topk_atoms()` function to `topk_gabor_atoms()`.

* Removed the `mp_omp_pipeline()` function because its functionality is now
  provided by `mp_omp_execute()`.
  
* Added `omp_reference()`, a simple educational and reference implementation
  of Orthogonal Matching Pursuit.

* Added and improved S3 methods for package objects, including methods for 
  printing, summarizing, and plotting.

* Updated package documentation, examples, README, and vignette to reflect
  the current MP-R, OMP-R, and EMPI workflows.

* Improved input validation, error handling, and overall code consistency.

# MatchingPursuit 1.2.0

* Added a native R implementation of the Matching Pursuit algorithm (`mp_core()`),
  intended primarily for educational, methodological, and experimental use. It
  complements the optimized external EMPI backend and allows the algorithm to be
  inspected and modified directly in R.

* Expanded the package vignette with examples of the native R Matching Pursuit
  implementation. The available decomposition backends are now more clearly
  distinguished, with the terms MP-R, OMP-R, and EMPI used consistently
  throughout.

* Significantly expanded `README.md` to provide a more comprehensive overview
  of the package, its functionality, and typical workflows.

* Renamed `plot.ecg()` and `read_ecg_signals() `to `plot.wfdb()` and `read_wfdb_signals()`, respectively.

* Updated `read_csv_signals()`, `read_edf_signals()`, and `read_wfdb_signals()`
  to return objects of classes `sig`, `edf`, and `wfdb`, respectively.

* Updated `empi_execute()`, `mp_omp_execute()`, and `topk_atoms()` to accept
  objects of classes `sig`, `edf`, and `wfdb`.
  
* Added `as_sig()` for converting signal data and a specified sampling frequency
  to an object of class `sig`.  

* Standardized function names to improve consistency across the package API:
  - `gabor_fun()` was renamed to `gabor_atom()`.
  - `sig2bin()` was renamed to `sig_to_bin()`.
  - `mp_omp_run_pipeline()` was renamed to `mp_omp_pipeline()`.
  - `read_dict()` was renamed to `read_gabor_dict()`.
  - `atom_params()` was renamed to `read_atom_params()`.
  - `filters_coeff()` was renamed to `design_filters()`.
  - `read_empi_db_file()` was renamed to `read_empi_db()`.
  - `sig_to_bin()` was renamed to `signal_to_bin()`.
  - `gabor_proj_fft()` was renamed to `gabor_projection_fft()`.

* Simplified `mp_omp_pipeline()` to provide a higher-level MP/OMP decomposition 
  workflow for signals stored in CSV format; signal-specific preprocessing is 
  expected to be performed separately.
  
* Improved input validation, error handling, and internal code robustness across
  several functions.  

# MatchingPursuit 1.1.0

* The project adopted a new naming convention for variables and functions, 
  replacing dot-separated names (`name.of.some.variable`) with snake_case 
  (`name_of_some_variable`).

* Implemented Orthogonal Matching Pursuit (OMP) and added support for XML-defined 
  dictionaries (functions: `read_dict()`, `topk_atoms()`, `omp_core()`, 
  `run_omp_run_pipeline()`, `omp_execute()` and `gabor_proj_fft()`). 

* Extended examples added to the package-level documentation and to the vignette.

* Added the ability to load data in WFDB (WaveForm DataBase) format.

* `plot_mp()` function was added.

* `plot_ecg()` function was added.

* `read_ecg_signals()` function was added.

* The `read_csv_files()` function also supports files where the channel names are 
  given in the second line.

* The `read_empi_db_file()` and `empi_execute()` functions now return object 
  of class `mp`.

* `empi2tf()` has been renamed to `tf_map()`, which provides support for both 
   the EMPI and OMP algorithms.

# MatchingPursuit 1.0.1

* Fixed a bug in the `empi2tf()` function that caused TF maps to be displayed 
  incorrectly for channels other than the first one (only applies to signals with 
  more than one channel).

* `clear.cache()` function. Before deleting files from the cache, it displays 
  a list of them and asks the user for permission to delete them.

* `empi.execute()` function. Additional validation has been added to ensure that 
  list items have the required names (`signal` and `sampling.rate`).

* `empi.install()` function. Added error handling for `download.file()` function.

# MatchingPursuit 1.0.0

* Initial CRAN submission.
