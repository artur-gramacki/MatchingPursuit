#' Processing Time Series Data Using the Matching Pursuit Algorithm
#'
#' Tools for analyzing and decomposing time-series data using the
#' \strong{Matching Pursuit (MP)} algorithm, a greedy signal decomposition technique that represents
#' complex signals as a linear combination of simpler functions (called atoms) selected from an
#' overcomplete dictionary.
#'
#' @details
#' In addition to working with generic time-series data, the package also supports direct loading of
#' data stored in EDF and EDF(+) files. These formats are widely used for storing physiological signals
#' such as EEG, EMG, or ECG recordings. By enabling the import of EDF and EDF(+) files, the package
#' facilitates the analysis of biomedical signals. The package uses the implementation of the Matching
#' Pursuit algorithm by \strong{Piotr Różański}, available at \url{https://github.com/develancer/empi}.
#'
#' Example datasets available through the \code{system.file()} function are:
#'
#'  \itemize{
#'    \item \code{EEG.edf}
#'      \itemize{
#'        \item 19 EEG channels + 1 EDF Annotations channel
#'        \item sampling frequency: 256 Hz, signal length: 10 sec.
#'        \item channel names: Fp1, Fp2, F3, F4, F7, F8, Fz, C3, C4, Cz, T3, T5, T4, T6, P3, P4, Pz, O1, O2, EDF_Annotations
#'      }
#'    \item \code{sample1.csv}
#'      \itemize{
#'        \item 1 channel
#'        \item sampling frequency: 1024 Hz, signal length: 1 sec.
#'      }
#'    \item \code{sample2.csv}
#'      \itemize{
#'        \item 1 channel
#'        \item sampling frequency: 128 Hz Hz, signal length: 10 sec.
#'      }
#' }
#' The first line of the \code{.csv} file contains two numbers: the sampling rate in Hz (\code{freq})
#' and the signal length in seconds (\code{sec}). \code{read.csv.signals} function checks
#' whether the file actually contains \code{freq*sec} samples.
#'
#' @docType package
#'
#' @name MatchingPursuit
#'
#' @references
#' Mallat, S. & Zhang, Z. (1993). \emph{Matching Pursuits with Time-Frequency Dictionaries}.
#' IEEE Transactions on Signal Processing, vol. 41, no. 12, pp. 3397-3415, \doi{10.1109/78.258082}
#'
#' Tropp, J.A. & Gilbert, A.C. (2007). \emph{Signal Recovery From Random Measurements via Orthogonal Matching}.
#' IEEE Transactions on Information Theory, vol. 53, no. 12, pp. 4655-4666, \doi{10.1109/TIT.2007.909108}
#'
#' Różański, P.T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with Continuous Dictionaries}.
#' ACM Transactions on Mathematical Software, vol.50, issue = 3, pp. 1-17, \doi{10.1145/3674832}
#'
#'  Gramacki, A. & Kunik, M. (2025).
#'  \emph{Deep learning epileptic seizure detection based on matching pursuit algorithm and its time-frequency graphical representation}. International Journal of Applied Mathematics & Computer Science, vol. 35, no. 4, pp. 617-630, \doi{10.61822/amcs-2025-0044}

#' @keywords internal
"_PACKAGE"
