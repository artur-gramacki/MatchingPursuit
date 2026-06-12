#' Reads WFDB-compatible signal and header files
#'
#' WFDB (WaveForm DataBase) is a standard file format for storing, reading,
#' and analyzing physiological time-series signals. It is widely used for
#' signals such as ECG, EEG, blood pressure, respiration, and other biomedical
#' waveforms. It was developed by PhysioNet and is commonly used in research datasets.
#'
#' A WFDB record typically consists of two main files:
#' \code{.dat} - binary signal samples (waveform values), and \code{.hea} - a header
#' file describing how to interpret the data. In some cases, additional annotation
#' files such as \code{.atr} may be present, containing beat labels or rhythm annotations.
#'
#' @param file Path to the ECG record to be read.
#'
#' @importFrom EGM read_wfdb
#' @importFrom tools file_path_sans_ext
#'
#' @return An object of class \code{ecg}. The returned value is a list containing:
#'   1) a matrix of signals stored in the ECG file,
#'   2) the sampling frequency,
#'   3) time stamps,
#'   4) lead names,
#'   5) record name.
#'
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#' dir <- dirname(file)
#' name <- tools::file_path_sans_ext(basename(file))
#'
#' out <- read_ecg_signals(file)
#' head(out$signal)
#' out$sampling_frequency
#' out$lead_names
#'
#' plot(out, begin = 0, end = 10, panel_height = 1.5)
read_ecg_signals <- function(file) {

  dir <- dirname(file)
  name <- tools::file_path_sans_ext(basename(file))

  out <- EGM::read_wfdb(
    record = name,
    record_dir = dir,
    units = "physical"
  )

  channels <- length(out$header$number)

  signal <- as.matrix(out$signal[, 2:(channels + 1)])

  lead_names <- colnames(signal)
  colnames(signal) <- lead_names

  sampling_frequency <- attr(out$header, "record_line")$frequency
  record_name <- attr(out$header, "record_line")$record_name

  time_stamps <- seq(0, by = 1 / sampling_frequency, length.out = nrow(signal))

  my_list <- list(
    signal = signal,
    time_stamps = time_stamps,
    sampling_frequency = sampling_frequency,
    lead_names = lead_names,
    record_name = record_name
  )

  class(my_list) <- "ecg"
  return(my_list)
}
