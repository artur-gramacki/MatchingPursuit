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
#' @return A list is returned with:
#' 1) data frame with all signals stored in the given ECG file,
#' 3) sampling rate,
#' 4) lead names,
#' 4) object of class \code{ecg}.
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#' dir <- dirname(file)
#' name <- tools::file_path_sans_ext(basename(file))
#'
#' out <- read.ecg.signals(file)
#' head(out$signals)
#' out$sampling.rate
#' out$lead.names
#'
read.ecg.signals <- function(file) {

  dir <- dirname(file)
  name <- tools::file_path_sans_ext(basename(file))

  out <- read_wfdb(
     record = name,
     record_dir = dir,
     units = "physical"
  )

  class(out) <- "ecg"

  channels <- length(out$header$number)
  ecg <- as.matrix(out$signal[, 2:(channels + 1)])
  lead.names <- colnames(ecg)
  colnames(ecg) <- lead.names
  fs <- attr(out$header, "record_line")$frequency

  return(list(
    signals = ecg,
    sampling.rate = fs,
    lead.names = lead.names,
    ecg.class = out)
  )
}
