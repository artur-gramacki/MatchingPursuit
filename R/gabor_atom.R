#' Generate a Gabor atom
#'
#' @description
#' Generates a real-valued Gabor atom consisting of a sinusoidal component
#' localized by a Gaussian envelope. Gabor atoms provide simultaneous
#' localization in time and frequency and are commonly used in
#' time-frequency dictionaries for Matching Pursuit decomposition.
#'
#' @param number_of_samples Positive integer specifying the number of samples
#'   in the generated Gabor atom.
#'
#' @param sampling_frequency Sampling frequency in Hz.
#'
#' @param mean Time position of the center of the Gaussian envelope, in seconds.
#'
#' @param phase Phase of the sinusoidal component, in radians.
#'
#' @param sigma Positive scale parameter controlling the width of the Gaussian
#'   envelope, in seconds.
#'
#' @param frequency Frequency of the sinusoidal component in Hz.
#'
#' @param normalization Logical; if \code{TRUE}, the resulting Gabor atom is
#'   normalized to unit Euclidean norm.
#'
#' @return A list containing four numeric vectors of length \code{number_of_samples}:
#'
#'   \item{cosine}{Cosine wave.}
#'   \item{gauss}{Gaussian envelope.}
#'   \item{gabor}{Gabor function.}
#'   \item{time}{Time vector corresponding to the signal samples.}
#'
#' @examples
#' number_of_samples <- 512
#' sampling_frequency <- 256.0
#' mean <- 1
#' phase <- pi
#' sigma <- 0.5
#' frequency <- 5.0
#' normalization = TRUE
#'
#' out <- gabor_atom(
#'   number_of_samples,
#'   sampling_frequency,
#'   mean,
#'   phase,
#'   sigma,
#'   frequency,
#'   normalization
#' )
#'
#' # Verify unit-norm normalization
#' sqrt(sum(out$gabor^2))
#'
#' plot(out$time, out$gabor, type = "l", xlab = "t", ylab = "gabor", panel.first = grid())
#'
#' @export
#'
gabor_atom <- function(
    number_of_samples,
    sampling_frequency,
    mean,
    phase,
    sigma,
    frequency,
    normalization = TRUE) {

  if (sigma <= 0) stop("'sigma' must be positive.")
  if (number_of_samples <= 0)  stop("'number_of_samples' must be positive.")
  if (sampling_frequency <= 0) stop("'sampling_frequency' must be positive.")

  if (frequency > sampling_frequency / 2) {
    stop("'frequency' must not exceed the Nyquist frequency.")
  }

  vec_norm <- function(x) { x / sqrt(sum(x^2)) }

  omega <- 2 * pi * frequency
  t <- seq(from = 0, to = number_of_samples - 1, by = 1) / sampling_frequency
  gauss <- exp(-pi * ((t - mean) / sigma)^2)
  cosine <- cos(omega * (t - mean) + phase)
  gabor <- cosine * gauss

  if (normalization) {
    gabor <- vec_norm(gabor)
  }

  list(
    cosine = cosine,
    gauss = gauss,
    gabor = gabor,
    time = t
  )
}
