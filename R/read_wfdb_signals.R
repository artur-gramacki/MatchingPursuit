#' Reads WFDB-compatible signal and header files
#'
#' WFDB (WaveForm DataBase) is a standard file format for storing, reading,
#' and analyzing physiological time-series signals. It is widely used for
#' signals such as ECG, EEG, blood pressure, respiration, and other biomedical
#' waveforms. It is the file format used by the PhysioNet project and is commonly
#' used in research datasets.
#'
#' A WFDB record typically consists of two main files:
#' \code{.dat} - binary signal samples (waveform values), and \code{.hea} - a header
#' file describing how to interpret the data. In some cases, additional annotation
#' files such as \code{.atr} may be present, containing beat labels or rhythm annotations.
#'
#' @param file Path to the WFDB record to be read.
#'
#' @importFrom EGM read_wfdb
#' @importFrom tools file_path_sans_ext
#'
#' @return An object of class \code{wfdb}. The returned value is a list containing:
#'
#' \describe{
#'   \item{signal}{Matrix of signals stored in the WFDB file.}
#'   \item{sampling_frequency}{Sampling frequency.}
#'   \item{time_stamps}{Time vector corresponding to signal samples.}
#'   \item{lead_names}{Names of the WFDB leads (channels).}
#'   \item{record_name}{Name of the file.}
#' }
#'
#' @note The function \code{EGM::read_wfdb()} from version 0.2.0 of the
#' \code{EGM} package does not support multi-frequency signals. Consequently,
#' records containing different numbers of samples per frame, as indicated by
#' the \code{16x2}, \code{16x4}, and \code{16x1} specifications below, cannot
#' be read correctly.
#'
#' \verb{
#' multi_freq_test 3 100 1000
#' multi_freq_test.dat 16x2 200.0(0)/mV 16 0 0 258 0 ECG
#' multi_freq_test.dat 16x4 400.0(0)/mmHg 16 0 400 57824 0 ABP
#' multi_freq_test.dat 16x1 100.0(0)/pm 16 0 0 18204 0 RESP
#' }
#'
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#'
#' out <- read_wfdb_signals(file)
#' head(out$signal)
#' out$sampling_frequency
#' out$lead_names
#'
#' plot(out, begin = 0, end = 10, panel_height = 1.5)
#'
read_wfdb_signals <- function(file) {

  if (!file.exists(file)) stop("File does not exist: ", file)

  dir <- dirname(file)
  name <- tools::file_path_sans_ext(basename(file))

  out <- EGM::read_wfdb(
    record = name,
    record_dir = dir,
    units = "physical"
  )

  channels <- length(out$header$number)

  # First column contains time, remaining columns are WFDB channels.
  signal <- as.matrix(out$signal[, 2:(channels + 1)])

  lead_names <- colnames(signal)

  sampling_frequency <- attr(out$header, "record_line")$frequency
  record_name <- attr(out$header, "record_line")$record_name

  time_stamps <- seq(0, by = 1 / sampling_frequency, length.out = nrow(signal))

  result <- list(
    signal = signal,
    sampling_frequency = sampling_frequency,
    time_stamps = time_stamps,
    lead_names = lead_names,
    record_name = record_name
  )

  class(result) <- "wfdb"
  return(result)
}
