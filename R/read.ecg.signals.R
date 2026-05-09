#' Reads WFDB-compatible signal & header files
#'
#' WFDB (WaveForm DataBase) is a standard file format for storing, reading,
#' and analyzing physiological time-series signals.It is widely used for signals
#' such as: ECG, EEG, blood pressure, respiration and other biomedical waveforms.
#' It was developed by PhysioNet and is common in research datasets.
#' A WFDB record usually contains two main files: \code{.dat} — binary signal
#' samples (the waveform values) and \code{.hea} — header file describing how to
#' interpret the data. Sometimes there are also annotation files such as
#' \code{.atr}, containing beat labels or rhythm annotations.
#'
#' @param file The path to the ECG record to be read.
#'
#' @importFrom EGM read_wfdb
#' @importFrom tools file_path_sans_ext
#'
#' @return An object of class \code{ecg}. The returned value is a list containing:
#'   1) a matrix with all signals stored in the ECG file,
#'   2) the sampling rate,
#'   3) the time stamps,
#'   4) the lead names,
#'   5) the record name.
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#' dir <- dirname(file)
#' name <- tools::file_path_sans_ext(basename(file))
#'
#' out <- read.ecg.signals(file)
#' head(out$signal)
#' out$sampling.rate
#' out$lead.names
#'
#' plot(out, begin = 0, end = 10, panel.height = 1.5)
#'
read.ecg.signals <- function(file) {

  dir <- dirname(file)
  name <- tools::file_path_sans_ext(basename(file))

  out <- EGM::read_wfdb(
     record = name,
     record_dir = dir,
     units = "physical"
  )

  channels <- length(out$header$number)
  signal <- as.matrix(out$signal[, 2:(channels + 1)])
  lead.names <- colnames(signal)
  colnames(signal) <- lead.names
  sampling.rate <- attr(out$header, "record_line")$frequency
  record.name <- attr(out$header, "record_line")$record_name

  time.stamps <- seq(0, by = 1 / sampling.rate, length.out = nrow(signal))

  my.list <- list(
    signal = signal,
    time.stamps = time.stamps,
    sampling.rate = sampling.rate,
    lead.names = lead.names,
    record.name = record.name
  )

  class(my.list) <- "ecg"
  return(my.list)
}
