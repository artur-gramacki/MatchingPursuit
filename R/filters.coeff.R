#' A wrapper function for \code{signal::butter} function
#'
#' @description
#' Implements notch, low-pass, high-pass, band-pass, and band-stop filters with desired frequency
#' ranges and Butterworth filter order.
#'
#' @param fs Sampling rate.
#'
#' @param notch Vector of two frequencies for notch filter.
#'
#' @param notch.order Notch filter order.
#'
#' @param lowpass Low-pass filter frequency.
#'
#' @param lowpass.order Low-pass filter order.
#'
#' @param highpass High-pass filter frequency.
#'
#' @param highpass.order High-pass filter order.
#'
#' @param bandpass Vector of two frequencies for band-pass filter.
#'
#' @param bandpass.order Band-pass filter order.
#'
#' @param bandstop Vector of two frequencies for band-stop filter.
#'
#' @param bandstop.order Band-stop filter order.
#'
#' @return List with parameters of individual filters.
#'
#' @importFrom signal butter freqz
#'
#' @export
#'
#' @examples
#' sampling.rate <- 250
#'
#' fc <- filters.coeff(
#'   fs = sampling.rate,
#'   notch = c(49, 51),
#'   lowpass = 40,
#'   highpass = 1,
#'   bandpass = c(0.5, 40),
#'   bandstop = c(10, 50)
#' )
#'
#' fc
#'
#' signal::freqz(fc$notch, Fs = sampling.rate)
#' signal::freqz(fc$low, Fs = sampling.rate)
#' signal::freqz(fc$high, Fs = sampling.rate)
#' signal::freqz(fc$bandpass, Fs = sampling.rate)
#' signal::freqz(fc$bandstop, Fs = sampling.rate)
#'
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' sigs <- read.edf.signals(file, resampling = FALSE)
#'
#' plot(sigs[, 1], type = "l", panel.first = grid())
#'
#' for (m in 1:ncol(sigs)) {
#'   sigs[, m] = signal::filtfilt(fc$notch, sigs[, m]); # 50Hz notch filter
#'   sigs[, m] = signal::filtfilt(fc$low, sigs[, m]); # Low pass IIR Butterworth
#'   sigs[, m] = signal::filtfilt(fc$high, sigs[, m]); # High pass IIR Butterwoth
#' }
#'
#' plot(sigs[, 1], type = "l", panel.first = grid())
#'
filters.coeff <- function (
    fs = 256,
    notch = c(49, 51), notch.order = 2,
    lowpass = 30, lowpass.order = 4,
    highpass = 1, highpass.order = 4,
    bandpass = c(0.5, 40), bandpass.order = 4,
    bandstop = c(0.5, 40), bandstop.order = 4)
{
  ## Notch filter
  notch <- butter(notch.order, notch / (fs / 2), "stop")

  # Low pass IIR Butterworth, cutoff at 'lowpass' Hz
  low <- butter(lowpass.order, lowpass / (fs / 2), "low")

  # High pass IIR Butterwoth, cutoff at 'highpass' Hz
  high <- butter(highpass.order, highpass / (fs / 2), "high")

  # Bandpass filter IIR Butterworth
  bandpass <- butter(bandpass.order, bandpass / (fs / 2), type = "pass")

  # Bandstop filter IIR Butterworth
  bandstop <- butter(bandstop.order, bandstop / (fs / 2), type = "stop")

  list(
    notch = notch,
    low = low,
    high = high,
    bandpass = bandpass,
    bandstop = bandstop)
}

