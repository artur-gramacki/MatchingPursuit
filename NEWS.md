# MatchingPursuit 1.1.0

* The project adopted a new naming convention for variables and functions, replacing dot-separated names (`name.of.some.variable`) with snake_case (`name_of_some_variable`).

* Implemented Orthogonal Matching Pursuit (OMP) and support for XML-defined dictionaries (functions: `read_dict()`, `topk_atoms()`, `omp_core()`, `run_omp_pipeline()`, `omp_execute()` and `gabor_proj_fft()`). 

* Extended examples added to the package-level documentation and to the vignette.

* Added the ability to load data in WFDB (WaveForm DataBase) format.

* `plot_mp()` function was added.

* `plot_ecg()` function was added.

* `read_ecg_signals()` function was added.

* The `read_csv_files()` function also supports files where the channel names are given in the second line.

* The `read_empi_db_file()` and `empi_execute()` functions now return object of class `mp`.

* `empi2tf()` has been renamed to `tf_map()`, which provides support for both the EMPI and OMP algorithms.

# MatchingPursuit 1.0.1

* Fixed a bug in the `empi2tf()` function that caused TF maps to be displayed incorrectly for channels other than the first one (only applies to signals with more than one channel).

* `clear.cache()` function. Before deleting files from the cache, it displays a list of them and asks the user for permission to delete them.

* `empi.execute()` function. Additional validation has been added to ensure that list items have the required names (`signal` and `sampling.rate`).

* `empi.install()` function. Added error handling for `download.file()` function.

# MatchingPursuit 1.0.0

* Initial CRAN submission.
