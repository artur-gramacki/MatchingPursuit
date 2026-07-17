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
#' @param file Path to the ECG record to be read.
#'
#' @importFrom EGM read_wfdb
#' @importFrom tools file_path_sans_ext
#'
#' @return An object of class \code{ecg}. The returned value is a list containing:
#'
#' \describe{
#'   \item{signal}{Matrix of signals stored in the ECG file.}
#'   \item{sampling_frequency}{Sampling frequency.}
#'   \item{time_stamps}{Time vector corresponding to signal samples.}
#'   \item{lead_names}{Names of the ECG leads (channels).}
#'   \item{record_name}{Name of the file.}
#' }
#'
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#'
#' out <- read_ecg_signals(file)
#' head(out$signal)
#' out$sampling_frequency
#' out$lead_names
#'
#' plot(out, begin = 0, end = 10, panel_height = 1.5)
#'
read_ecg_signals <- function(file) {

  if (!file.exists(file)) stop("File does not exist: ", file)

  dir <- dirname(file)
  name <- tools::file_path_sans_ext(basename(file))

  out <- EGM::read_wfdb(
    record = name,
    record_dir = dir,
    units = "physical"
  )

  channels <- length(out$header$number)

  # First column contains time, remaining columns are ECG channels.
  signal <- as.matrix(out$signal[, 2:(channels + 1)])

  lead_names <- colnames(signal)

  sampling_frequency <- attr(out$header, "record_line")$frequency
  record_name <- attr(out$header, "record_line")$record_name

  time_stamps <- seq(0, by = 1 / sampling_frequency, length.out = nrow(signal))

  result <- list(
    signal = signal,
    time_stamps = time_stamps,
    sampling_frequency = sampling_frequency,
    lead_names = lead_names,
    record_name = record_name
  )

  class(result) <- "ecg"
  return(result)
}
