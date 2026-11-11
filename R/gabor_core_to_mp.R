#' Convert a Gabor MP/OMP core result to an mp object
#'
#' @description
#' Converts the output of \code{mp_core_gabor()} or \code{omp_core_gabor()}
#' into a standardized object of class \code{"mp"}.
#'
#' @details
#' The function transforms a single-channel Gabor decomposition result
#' into the object structure used by \code{mp_omp_execute()}.
#' The channel identifier in the resulting object is always set to 1.
#' It preserves the original signal, reconstructed signal, selected atoms,
#' atom energies, and time-frequency parameters, including frequency,
#' phase, scale, and position.
#' The reconstruction is obtained by multiplying the matrix of selected
#' atoms by their corresponding coefficients.
#'
#' The resulting object can be used with functions that accept objects
#' of class \code{"mp"}, including time-frequency visualization tools
#' such as \code{tf_map()}.
#'
#' This function does not perform signal decomposition. It only converts
#' an existing decomposition result into the standard \code{"mp"} format.
#'
#' The function is intended exclusively for the Gabor-specific core
#' implementations. It is not designed for the general-purpose
#' \code{mp_core()} and \code{omp_core()} functions.
#'
#' @param core A list returned by \code{mp_core_gabor()} or
#'   \code{omp_core_gabor()}. It must contain the selected atoms,
#'   signal, coefficients, atom energies, and Gabor parameters.
#'
#' @param sampling_frequency Numeric scalar specifying the sampling
#'   frequency of the analyzed signal in Hz.
#'
#' @return
#' An object of class \code{"mp"} containing:
#' \describe{
#'   \item{atoms}{Data frame containing the channel identifier, atom
#'     number, energy, envelope type, frequency, phase, scale, and position
#'     of each selected Gabor atom.}
#'   \item{signal}{Numeric matrix containing the original signal
#'     (samples \eqn{\times} 1).}
#'   \item{reconstruction}{Numeric matrix containing the reconstructed
#'     signal (samples \eqn{\times} 1).}
#'   \item{selected_atoms}{List containing the matrix of selected Gabor
#'     atoms for the analyzed channel.}
#'   \item{time}{Numeric vector containing the time coordinates
#'     of signal samples in seconds.}
#'   \item{sampling_frequency}{Sampling frequency in Hz.}
#' }
#'
#' @seealso
#' \code{\link{mp_core_gabor}},
#' \code{\link{omp_core_gabor}},
#' \code{\link{mp_omp_execute}},
#' \code{\link{tf_map}}
#'
#' @export
#'
#' @examples
#' # Generate a Gabor dictionary
#' sampling_frequency <- 128
#' duration <- 1
#' t <- seq(0, duration - 1 / sampling_frequency, by = 1 / sampling_frequency)
#' y <- sin(2 * pi * 20 * t) + 0.5 * sin(2 * pi * 50 * t)
#' sig <- as_sig(y, sampling_frequency)
#'
#' xml_file <- tempfile(fileext = ".xml")
#' generate_xml_dict(length(y), xml_file)
#'
#' atoms_dict <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = sampling_frequency,
#'   duration = duration
#' )
#'
#' # MP decomposition
#' fit_mp <- mp_core_gabor(
#'   atoms_dict = atoms_dict,
#'   signal = sig,
#'   channel = 1,
#'   n_nonzero_coefs = 20,
#'   verbose = FALSE
#' )
#'
#' out_mp <- gabor_core_to_mp(fit_mp, sampling_frequency)
#'
#' # OMP decomposition
#' fit_omp <- omp_core_gabor(
#'   atoms_dict = atoms_dict,
#'   signal = sig,
#'   channel = 1,
#'   n_nonzero_coefs = 20,
#'   verbose = FALSE
#' )
#'
#' out_omp <- gabor_core_to_mp(fit_omp, sampling_frequency)
#'
#' # Verify the resulting object
#' class(out_omp)
#' head(out_omp$atoms)
#'
#' # Verify reconstruction consistency
#' max(abs(out_omp$reconstruction[, 1] - fit_omp$reconstruction))
#'
gabor_core_to_mp <- function(core, sampling_frequency) {

  # Validate input ----
  required <- c("selected_atoms", "signal", "coefs", "energy",
                "frequency", "phase", "scale", "position")

  if (!all(required %in% names(core))) {
    stop("Invalid Gabor core result: missing required fields.")
  }

  selected_atoms <- as.matrix(core$selected_atoms)
  n_atoms <- ncol(selected_atoms)
  n_samples <- nrow(selected_atoms)

  if (any(vapply(core[c("coefs", "energy", "frequency", "phase", "scale", "position")],
                 length, integer(1)) != n_atoms)) {
    stop("Inconsistent number of selected atoms and parameters.")
  }

  if (length(core$signal) != n_samples) stop("Signal length mismatch.")
  if (length(sampling_frequency) != 1L || !is.finite(sampling_frequency) ||
      sampling_frequency <= 0) stop("Invalid sampling frequency.")

  # Atom metadata ----
  atoms <- data.frame(
    channel_id = rep(1L, n_atoms),
    atom_number = seq_len(n_atoms),
    energy = core$energy,
    envelope = rep("gauss", n_atoms),
    frequency = core$frequency,
    phase = core$phase,
    scale = core$scale,
    position = core$position
  )

  # Build mp object ----
  out <- list(
    atoms = atoms,
    signal = matrix(as.numeric(core$signal), ncol = 1),
    reconstruction = matrix(as.numeric(selected_atoms %*% core$coefs), ncol = 1),
    selected_atoms = list(selected_atoms),
    time = (seq_len(n_samples) - 1) / sampling_frequency,
    sampling_frequency = sampling_frequency
  )

  class(out) <- "mp"
  return(out)
}

