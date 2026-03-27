#' Gabor function implementation
#'
#' @description
#' Gabor function is a sinusoidal wave localized by a Gaussian envelope. In signal processing it is
#' widely used as a basic building block for representing signals that are localized in both time and
#' frequency. Matching Pursuit algorithm uses a redundant dictionary of the so called \emph{Gabor atoms}.
#' Gabor atoms are ideal for Matching Pursuit because they: 1) provide optimal time–frequency
#' localization, 2) represent oscillatory signals well, 3) enable adaptive time-frequency decomposition.
#'
#' @param number.of.samples How many samples should the generated atom consist of?
#' @param sampling.frequency Sampling frequency.
#' @param mean Time position.
#' @param phase Phase.
#' @param sigma Scale / width of the Gaussian window.
#' @param frequency Frequency of the sinusoid.
#' @param normalization If \code{TRUE}, norm of the generated atom equals 1.
#'
#' @return List of 4 vectors with cosine, gauss, gabor and time waveforms of size \code{number.of.samples}.
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
  v <- exp(-pi * ((t - mean) / sigma)^2)
  u <- cos(omega * (t - mean) + phase)
  gabor <- u * v
  if (normalization) {
    gabor <- vec.norm(gabor)
  }
  list(cosinus = u, gauss = v, gabor = gabor, t = t)
}
