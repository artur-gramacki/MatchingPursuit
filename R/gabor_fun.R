#' Gabor function implementation
#'
#' @description
#' A Gabor function is a sinusoidal wave localized by a Gaussian envelope. In signal processing, it is
#' widely used as a basic building block for representing signals localized in both time and frequency.
#' The Matching Pursuit algorithm uses a redundant dictionary of so-called \emph{Gabor atoms}.
#' These atoms are particularly suitable because they: 1) provide optimal time–frequency
#' localization, 2) represent oscillatory signals well, 3) enable adaptive time-frequency decomposition.
#'
#' @param number_of_samples Number of samples in the generated atom.
#' @param sampling_frequency Sampling frequency.
#' @param mean Time position of the Gaussian envelope.
#' @param phase Phase of the sinusoidal component.
#' @param sigma Scale parameter controlling the width of the Gaussian window.
#' @param frequency Frequency of the sinusoidal component.
#' @param normalization If \code{TRUE}, the resulting atom is normalized to have unit norm.
#'
#' @return A list containing four numeric vectors of length \code{number_of_samples}:
#'
#'   \item{cosine}{Cosine wave.}
#'   \item{gauss}{Gaussian envelope.}
#'   \item{gabor}{Gabor function.}
#'   \item{t}{Time vector corresponding to the signal samples.}
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
#' out <- gabor_fun(
#'   number_of_samples,
#'   sampling_frequency,
#'   mean,
#'   phase,
#'   sigma,
#'   frequency,
#'   normalization
#' )
#'
#' # If normalization = TRUE, norm of atom = 1, we can check it
#' crossprod(out$gabor)
#'
#' plot(out$t, out$gabor, type = "l", xlab = "t", ylab = "gabor", panel.first = grid())
#'
#' @export
#'
gabor_fun <- function(
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
    t = t
  )
}
