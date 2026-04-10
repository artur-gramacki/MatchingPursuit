# MatchingPursuit 1.0.1

* Fixed a bug in the `empi2tf()` function that caused TF maps to be displayed incorrectly for channels other than the first one (only applies to signals with more than one channel).

* The `clear.cache()` function has been changed. Before deleting files from the cache, it displays a list of them and asks the user for permission to delete them.

* The `empi.execute()` function has been changed. Additional validation has been added to ensure that list items have the required names (`signal` and `sampling.rate`).

* The `empi.install()` function has been changed. Added error handling for `download.file()` function.

# MatchingPursuit 1.0.0

* Initial CRAN submission.
