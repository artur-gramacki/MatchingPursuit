#' A wrapper function for \code{signal::butter()} function
#'
#' @description
#' Implements notch, low-pass, high-pass, band-pass, and band-stop filters
#' with specified frequency ranges and Butterworth filter order.
#'
#' @param sf Sampling frequency.
#'
#' @param notch Vector of two frequencies for notch filter.
#'
#' @param notch_order Notch filter order.
#'
#' @param lowpass Low-pass filter frequency.
#'
#' @param lowpass_order Low-pass filter order.
#'
#' @param highpass High-pass filter frequency.
#'
#' @param highpass_order High-pass filter order.
#'
#' @param bandpass Vector of two frequencies for band-pass filter.
#'
#' @param bandpass_order Band-pass filter order.
#'
#' @param bandstop Vector of two frequencies for band-stop filter.
#'
#' @param bandstop_order Band-stop filter order.
#'
#' @return
#' List containing coefficients of individual Butterworth filters.
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
#' fc <- filters_coeff(
#'   sf = sampling_frequency,
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
filters_coeff <- function (
    sf = 256,
    notch = c(49, 51), notch_order = 2,
    lowpass = 30, lowpass_order = 4,
    highpass = 1, highpass_order = 4,
    bandpass = c(0.5, 40), bandpass_order = 4,
    bandstop = c(0.5, 40), bandstop_order = 4)
{

  if (sf <= 0) stop("Sampling frequency must be positive.")

  nyq <- sf / 2

  if (any(notch <= 0 | notch >= nyq)) stop("Notch frequencies must be between 0 and Nyquist frequency.")

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
