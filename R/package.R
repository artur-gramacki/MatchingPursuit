#' Processing Time Series Data Using the Matching Pursuit Algorithm
#'
#' This package provides tools for analysing and decomposing time series data using the \strong{Matching Pursuit (MP)} algorithm, a greedy signal decomposition technique that represents complex signals as a linear combination of simpler functions selected from an overcomplete dictionary.
#'
#' @details
#' In addition to working with generic time-series data, the package also supports direct loading of data stored in EDF and EDF(+) files. These formats are widely used for storing physiological signals such as EEG, EMG, or ECG recordings. By enabling the import of EDF and EDF(+) files, the package facilitates the analysis of biomedical signals.
#'
#' The package uses the implementation of the Matching Pursuit algorithm by \strong{Piotr Różański}, available at \url{https://github.com/develancer/empi}.
#'
#' @docType package
#'
#' @name MatchingPursuit
#'
#' @references
#' Mallat, S. & Zhang, Z. (1993). \emph{Matching Pursuits with Time-Frequency Dictionaries}.
#' IEEE Transactions on Signal Processing, vol. 41, no. 12, pp. 3397-3415, \url{https://doi.org/10.1109/78.258082}
#'
#' Tropp, J.A. & Gilbert, A.C. (2007). \emph{Signal Recovery From Random Measurements via Orthogonal Matching}.
#' IEEE Transactions on Information Theory, vol. 53, no. 12, pp. 4655-4666, \url{https://doi.org/10.1109/TIT.2007.909108}
#'
#' Różański, P.T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with Continuous Dictionaries}.
#' ACM Transactions on Mathematical Software, vol.50, issue = 3, pp. 1-17, \url{https://doi.org/10.1145/3674832}
#'
#'  Gramacki, A. & Kunik, M. (2025).
#'  \emph{Deep learning epileptic seizure detection based on matching pursuit algorithm and its time-frequency graphical representation}. International Journal of Applied Mathematics & Computer Science, vol. 35, no. 4, pp. 617-630, \url{https://doi.org//10.61822/amcs-2025-0044}

#' @keywords internal
"_PACKAGE"
