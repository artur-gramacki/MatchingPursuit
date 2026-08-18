#' Convert a Signal to a \code{sig} Object
#'
#' Creates an object of class \code{sig} from signal data already available
#' in \proglang{R}. The function provides a convenient way to prepare signals
#' for subsequent processing and decomposition without importing them from
#' a file.
#'
#' @param signal A numeric vector, matrix, or data frame containing the signal
#'   values. For multi-channel signals, individual channels are assumed to be
#'   stored in columns.
#'
#' @param sampling_frequency A single positive numeric value specifying the
#'   sampling frequency of the signal in Hz.
#'
#' @return An object of class \code{sig}, which is a list containing:
#' \itemize{
#'   \item \code{signal}: A data frame containing the signal values.
#'   \item \code{sampling_frequency}: The sampling frequency in Hz.
#'   \item \code{time}: A numeric vector containing the time coordinates
#'     of the signal samples in seconds, starting at 0.
#' }
#'
#' @details
#' The time vector is generated automatically from the number of signal
#' samples and the specified sampling frequency. The resulting object has
#' the same basic structure as objects returned by
#' \code{read_csv_signals()}.
#'
#' @export
#'
#' @seealso
#' \code{\link{read_csv_signals}},
#' \code{\link{read_edf_signals}},
#' \code{\link{read_wfdb_signals}}
#'
#' @examples
#' # Single-channel signal
#' x <- rnorm(1000)
#' sig <- as_sig(x, sampling_frequency = 100)
#' str(sig)
#'
#' # Multi-channel signal
#' x <- cbind(
#'   channel1 = rnorm(1000),
#'   channel2 = rnorm(1000)
#' )
#' sig <- as_sig(x, sampling_frequency = 100)
#' str(sig)
#'
as_sig <- function(signal, sampling_frequency) {

  if (!is.numeric(sampling_frequency) ||
      length(sampling_frequency) != 1L ||
      !is.finite(sampling_frequency) ||
      sampling_frequency <= 0) {
    stop("'sampling_frequency' must be a single positive finite numeric value.")
  }

  if (!(is.vector(signal) || is.matrix(signal) || is.data.frame(signal))) {
    stop("'signal' must be a vector, matrix, or data frame.")
  }

  signal <- as.data.frame(signal)

  if (!all(vapply(signal, is.numeric, logical(1)))) {
    stop("All signal channels must be numeric.")
  }

  time <- seq(0, nrow(signal) - 1) / sampling_frequency

  structure(
    list(
      signal = signal,
      sampling_frequency = sampling_frequency,
      time = time
    ),
    class = "sig"
  )
}
