#' Design Butterworth filters
#'
#' @description
#' Designs notch, low-pass, high-pass, band-pass, and band-stop
#' Butterworth filters for a specified sampling frequency.
#'
#' @param sampling_frequency Sampling frequency in Hz.
#'
#' @param notch Numeric vector of length two specifying the lower and upper
#'   cutoff frequencies of the notch filter in Hz.
#'
#' @param notch_order Positive integer specifying the notch filter order.
#'
#' @param lowpass Numeric value specifying the low-pass cutoff frequency in Hz.
#'
#' @param lowpass_order Positive integer specifying the low-pass filter order.
#'
#' @param highpass Numeric value specifying the high-pass cutoff frequency in Hz.
#'
#' @param highpass_order Positive integer specifying the high-pass filter order.
#'
#' @param bandpass Numeric vector of length two specifying the lower and upper
#'   cutoff frequencies of the band-pass filter in Hz.
#'
#' @param bandpass_order Positive integer specifying the band-pass filter order.
#'
#' @param bandstop Numeric vector of length two specifying the lower and upper
#'   cutoff frequencies of the band-stop filter in Hz.
#'
#' @param bandstop_order Positive integer specifying the band-stop filter order.
#'
#' @return
#' A list containing the designed Butterworth filter objects:
#'
#' \describe{
#'   \item{notch}{Notch filter used to remove a specific narrow frequency band.}
#'   \item{lowpass}{Low-pass filter that attenuates high-frequency components.}
#'   \item{highpass}{High-pass filter that attenuates low-frequency components.}
#'   \item{bandpass}{Band-pass filter that retains frequencies within a selected range.}
#'   \item{bandstop}{Band-stop filter that removes frequencies within a selected range.}
#' }
#'
#' @importFrom signal butter freqz
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' out <- read_edf_signals(file, resampling = FALSE)
#' signal <- out$signal
#' sampling_frequency <- out$sampling_frequency
#'
#' fc <- design_filters(
#'   sampling_frequency = sampling_frequency,
#'   notch = c(49, 51),
#'   lowpass = 40,
#'   highpass = 1,
#'   bandpass = c(0.5, 40),
#'   bandstop = c(10, 50)
#' )
#'
#' print(fc)
#'
#' signal::freqz(fc$notch, Fs =  sampling_frequency)
#' signal::freqz(fc$lowpass, Fs =  sampling_frequency)
#' signal::freqz(fc$highpass, Fs =  sampling_frequency)
#' signal::freqz(fc$bandpass, Fs =  sampling_frequency)
#' signal::freqz(fc$bandstop, Fs =  sampling_frequency)
#'
#' plot(signal[, 1], type = "l", panel.first = grid())
#'
#' signal_filt <- signal
#'
#' for (m in 1:ncol(signal)) {
#'   signal_filt[, m] <- signal::filtfilt(fc$notch, signal_filt[, m]); # 50Hz notch filter
#'   signal_filt[, m] <- signal::filtfilt(fc$lowpass, signal_filt[, m]); # Low pass IIR Butterworth
#'   signal_filt[, m] <- signal::filtfilt(fc$highpass, signal_filt[, m]); # High pass IIR Butterwoth
#' }
#'
#' plot(signal_filt[, 1], type = "l", panel.first = grid())
#'
design_filters <- function (
    sampling_frequency = 256,
    notch = c(49, 51), notch_order = 2,
    lowpass = 30, lowpass_order = 4,
    highpass = 1, highpass_order = 4,
    bandpass = c(0.5, 40), bandpass_order = 4,
    bandstop = c(0.5, 40), bandstop_order = 4)
{

  if (!is.numeric(sampling_frequency) ||
      length(sampling_frequency) != 1L ||
      !is.finite(sampling_frequency) ||
      sampling_frequency <= 0) {
    stop("'sampling_frequency' must be a single positive number.")
  }

  nyq <- sampling_frequency / 2

  check_single_frequency <- function(x, name) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        !is.finite(x) ||
        x <= 0 ||
        x >= nyq) {
      stop("'", name, "' must be between 0 and the Nyquist frequency.")
    }
  }

  check_band <- function(x, name) {
    if (!is.numeric(x) ||
        length(x) != 2L ||
        any(!is.finite(x)) ||
        any(x <= 0) ||
        any(x >= nyq) ||
        x[1] >= x[2]) {
      stop(
        "'", name,
        "' must contain two increasing frequencies between 0 and the Nyquist frequency."
      )
    }
  }

  check_order <- function(x, name) {
    if (!is.numeric(x) ||
        length(x) != 1L ||
        !is.finite(x) ||
        x < 1 ||
        x != as.integer(x)) {
      stop("'", name, "' must be a positive integer.")
    }
  }

  check_band(notch, "notch")
  check_single_frequency(lowpass, "lowpass")
  check_single_frequency(highpass, "highpass")
  check_band(bandpass, "bandpass")
  check_band(bandstop, "bandstop")

  check_order(notch_order, "notch_order")
  check_order(lowpass_order, "lowpass_order")
  check_order(highpass_order, "highpass_order")
  check_order(bandpass_order, "bandpass_order")
  check_order(bandstop_order, "bandstop_order")

  ## Notch filter
  notch <- butter(notch_order, notch / nyq, "stop")

  # Low pass IIR Butterworth, cutoff at 'lowpass' Hz
  lowpass <- butter(lowpass_order, lowpass / nyq, "low")

  # High pass IIR Butterworth, cutoff at 'highpass' Hz
  highpass <- butter(highpass_order, highpass / nyq, "high")

  # Bandpass filter IIR Butterworth
  bandpass <- butter(bandpass_order, bandpass / nyq, type = "pass")

  # Bandstop filter IIR Butterworth
  bandstop <- butter(bandstop_order, bandstop / nyq, type = "stop")

  list(
    notch = notch,
    lowpass = lowpass,
    highpass = highpass,
    bandpass = bandpass,
    bandstop = bandstop
  )
}
