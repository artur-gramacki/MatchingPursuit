#' Gabor function implementation
#'
#' @description
#' A Gabor function is a sinusoidal wave localized by a Gaussian envelope. In signal processing, it is
#' widely used as a basic building block for representing signals localized in both time and frequency.
#' The Matching Pursuit algorithm uses a redundant dictionary of so-called \emph{Gabor atoms}.
#' These atoms are particularly suitable because they: 1) provide optimal time–frequency
#' localization, 2) represent oscillatory signals well, 3) enable adaptive time-frequency decomposition.
#'
#' @param number.of.samples Number of samples in the generated atom.
#' @param sampling.frequency Sampling frequency.
#' @param mean Time position of the Gaussian envelope.
#' @param phase Phase of the sinusoidal component.
#' @param sigma Scale parameter controlling the width of the Gaussian window.
#' @param frequency Frequency of the sinusoidal component.
#' @param normalization If \code{TRUE}, the resulting atom is normalized to have unit norm.
#'
#' @return A list containing four numeric vectors of length \code{number.of.samples}:
#' cosine, Gaussian envelope, Gabor function, and time axis.
#'
#' @examples
#' number.of.samples <- 512
#' sampling.frequency <- 256.0
#' mean <- 1
#' phase <- pi
#' sigma <- 0.5
#' frequency <- 5.0
#' normalization = TRUE
#'
#' out <- gabor.fun(
#'   number.of.samples,
#'   sampling.frequency,
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
gabor.fun <- function(
    number.of.samples,
    sampling.frequency,
    mean,
    phase,
    sigma,
    frequency,
    normalization = TRUE) {

  vec.norm <- function(x) {x / sqrt(sum(x^2))}

  omega <- 2 * pi * frequency
  t <- seq(from = 0, to = number.of.samples - 1, by = 1) / sampling.frequency
  gauss <- exp(-pi * ((t - mean) / sigma)^2)
  cosinus <- cos(omega * (t - mean) + phase)
  gabor <- cosinus * gauss
  if (normalization) {
    gabor <- vec.norm(gabor)
  }
  list(cosinus = cosinus, gauss = gauss, gabor = gabor, t = t)
}
