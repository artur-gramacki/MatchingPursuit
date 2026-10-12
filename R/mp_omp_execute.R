#' Matching Pursuit (MP) or Orthogonal Matching Pursuit (OMP) decomposition
#' for multi-channel signals
#'
#' Performs sparse signal decomposition using either the Matching Pursuit (MP) or
#' Orthogonal Matching Pursuit (OMP) algorithm, as specified by the \code{mode}
#' parameter. The decomposition is performed independently for each signal channel
#' using a parameterized Gabor dictionary without materializing the complete
#' dictionary as a time-domain matrix.
#'
#' The returned object is of class \code{"mp"} and can be visualized using
#' \code{plot()} and \code{tf_map()}.
#'
#' @param signal An object of class \code{sig} returned by \code{read_csv_signals()},
#' an object of class \code{edf} returned by \code{read_edf_signals()},
#' or an object of class \code{wfdb} returned by \code{read_wfdb_signals()}.
#'
#' @param mode \code{"omp"} or \code{"mp"}. Specifies the algorithm to use
#' for signal decomposition.
#'
#' @param dictionary Character string specifying the path to an XML file
#' containing the dictionary specification. It can be generated using
#' \code{generate_xml_dict()} or prepared manually by the user. If \code{NULL},
#' the XML file is generated internally by the function.
#'
#' @param full_atoms_in_signal Logical. If \code{TRUE}, only candidate atoms
#' whose complete support lies within the signal are included in the dictionary.
#' If \code{FALSE}, candidate atom supports may extend beyond the signal
#' boundaries.
#'
#' @param n_nonzero_coefs Optional positive integer specifying the maximum
#' number of selected atoms for each signal channel when \code{tol = NULL}.
#' If both \code{n_nonzero_coefs} and \code{tol} are \code{NULL}, a default
#' of 50 atoms is used.
#'
#' @param tol Optional stopping tolerance for the relative residual energy,
#' between 0 and 1. If specified, it overrides \code{n_nonzero_coefs}.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed
#' during processing.
#'
#' @return An object of class \code{"mp"} containing:
#'
#' \item{atoms}{A data frame describing the selected atoms.}
#' \item{signal}{Matrix containing the original signal(s).}
#' \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#' \item{selected_atoms}{List of matrices containing selected unit-L2-normalized
#'   atoms for each channel.}
#' \item{time}{Time vector corresponding to signal samples.}
#' \item{sampling_frequency}{Sampling frequency.}
#'
#' The \code{atoms} data frame contains:
#'
#' \itemize{
#' \item \code{channel_id} — signal channel identifier,
#' \item \code{atom_number} — atom index within the channel,
#' \item \code{energy} — atom energy contribution,
#' \item \code{envelope} — envelope type,
#' \item \code{frequency} — atom frequency (Hz),
#' \item \code{phase} — atom phase (radians),
#' \item \code{scale} — atom scale (seconds),
#' \item \code{position} — atom centre position (seconds).
#' }
#'
#' @details
#' The XML dictionary specification is first processed by
#' \code{read_gabor_dict()}. For each signal channel, the selected algorithm
#' iteratively searches the parameterized Gabor dictionary for the atom
#' best matching the current residual. Candidate projections are evaluated
#' using FFT-based computations, and the phase of each candidate is optimized
#' analytically.
#'
#' In MP mode, \code{mp_core_gabor()} updates the residual by subtracting
#' the contribution of the selected atom at each iteration. In OMP mode,
#' \code{omp_core_gabor()} additionally recomputes the coefficients of all
#' selected atoms by least squares using an incremental Cholesky factorization.
#'
#' The complete time-domain dictionary is never stored in memory. Only the
#' selected Gabor atoms are reconstructed and retained. The decomposition
#' stops when the specified number of atoms has been selected or, if
#' \code{tol} is provided, when the relative residual energy reaches the
#' requested tolerance, subject to the algorithm's iteration limit.
#'
#' The results from all channels are combined into an object of class
#' \code{"mp"}, which can be visualized using \code{plot()} and
#' \code{tf_map()}.
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{generate_xml_dict}},
#' \code{\link{omp_core_gabor}},
#' \code{\link{mp_core_gabor}}
#'
#' @export
#'
#' @examples
#' # +-------------------------------------------------------------+
#' # | Read signal                                                 |
#' # +-------------------------------------------------------------+
#' file <- system.file(
#'   "extdata",
#'   "sample3.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' signal <- read_csv_signals(
#'   file,
#'   col_names_in_csv = TRUE
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Run Matching Pursuit (MP-R backend)                         |
#' # +-------------------------------------------------------------+
#' fit_mp <- mp_omp_execute(
#'   mode = "mp",
#'   signal = signal,
#'   n_nonzero_coefs = 10,
#'   verbose = TRUE
#' )
#'
#' plot(fit_mp, channel = 3)
#'
#' # +-------------------------------------------------------------+
#' # | Run Orthogonal Matching Pursuit (OMP-R backend)             |
#' # +-------------------------------------------------------------+
#' fit_omp <- mp_omp_execute(
#'   mode = "omp",
#'   signal = signal,
#'   n_nonzero_coefs = 10,
#'   verbose = TRUE
#' )
#'
#' plot(fit_omp, channel = 3)
#'
#' # +-------------------------------------------------------------+
#' # | Stop decomposition using residual energy tolerance         |
#' # +-------------------------------------------------------------+
#' fit_mp_tol <- mp_omp_execute(
#'   mode = "mp",
#'   signal = signal,
#'   tol = 0.1,
#'   verbose = TRUE
#' )
#'
#' plot(fit_mp_tol, channel = 3)
#'
#' # +-------------------------------------------------------------+
#' # | Restrict Gabor atoms to the signal boundaries               |
#' # +-------------------------------------------------------------+
#' fit_mp_full <- mp_omp_execute(
#'   mode = "mp",
#'   signal = signal,
#'   full_atoms_in_signal = TRUE,
#'   n_nonzero_coefs = 10,
#'   verbose = TRUE
#' )
#'
#' plot(fit_mp_full, channel = 3)
#'
#' # +-------------------------------------------------------------+
#' # | Use an external XML dictionary specification                |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample1.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' fit_mp_xml <- mp_omp_execute(
#'   mode = "mp",
#'   signal = signal,
#'   dictionary = xml_file,
#'   n_nonzero_coefs = 10,
#'   verbose = TRUE
#' )
#'
#' plot(fit_mp_xml, channel = 3)
#'
mp_omp_execute <- function (
    signal,
    mode = NULL,
    dictionary = NULL,
    full_atoms_in_signal = FALSE,
    n_nonzero_coefs = NULL,
    tol = NULL,
    verbose = FALSE
) {

  if (is.null(mode)) {
    stop("'mode' must be specified.")
  }

  if (!mode %in% c("mp", "omp")) {
    stop("'mode' must be either 'mp' or 'omp'.")
  }

  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig <- as.matrix(signal$signal)
  sampling_frequency <- signal$sampling_frequency
  signal_length <- nrow(signal$signal)
  duration <- nrow(signal$signal) / sampling_frequency

  if (is.null(dictionary)) {
    xml_file <- tempfile(fileext = ".xml")
    generate_xml_dict(N = signal_length, file = xml_file)
  } else {
    xml_file <- dictionary
  }

  atoms_dict <- read_gabor_dict(
    xml_file = xml_file,
    sampling_frequency = sampling_frequency,
    duration = duration,
    full_atoms_in_signal = full_atoms_in_signal,
    verbose = verbose
  )

  channel_id <- c()
  atom_number <- c()
  energy <- c()
  envelope <- c()
  frequency <- c()
  phase <- c()
  scale <- c()
  position <- c()
  selected_atoms <-list()

  results <- vector("list", ncol(sig))

  for (ch in seq_len(ncol(sig))) {

    if (mode == "omp") {
      res <- omp_core_gabor(
        atoms_dict = atoms_dict,
        signal = signal,
        channel = ch,
        tol = tol,
        n_nonzero_coefs = n_nonzero_coefs,
        sigma_divisor = NULL,
        verbose = verbose
      )
    }

    if (mode == "mp") {
      res <- mp_core_gabor(
        atoms_dict = atoms_dict,
        signal = signal,
        channel = ch,
        tol = tol,
        n_nonzero_coefs = n_nonzero_coefs,
        sigma_divisor = NULL,
        verbose = verbose
      )
    }

    results[[ch]] <- res

    num_atoms <- ncol(results[[ch]]$selected_atoms)

    fr <- res$frequency
    ph <- res$phase
    sc <- res$scale
    po <- res$position

    i1 <- rep(ch, num_atoms)
    channel_id <- c(channel_id, i1)

    i2 <- seq(1, num_atoms)
    atom_number <- c(atom_number, i2)

    i3 <- results[[ch]]$energy
    energy <- c(energy, i3)

    i4 <- rep("gauss", num_atoms)
    envelope <-c(envelope, i4)

    i5 <- fr
    frequency <-c(frequency, i5)

    i6 <- ph
    phase <-c(phase, i6)

    i7 <- sc
    scale <-c(scale, i7)

    i8 <- po
    position <-c(position, i8)

    if (verbose) {
      message("mp_omp_execute(): method = \"", mode,
              "\", channel = ", ch, " successfully processed.")
    }
  }

  n_channels <- length(results)
  n_samples <- nrow(results[[1]]$selected_atoms)
  time <- seq(0, (n_samples - 1) / sampling_frequency, by = 1 / sampling_frequency)

  original_signal <- matrix(NA, nrow = n_samples, ncol = n_channels)
  reconstruction <- matrix(NA, nrow = n_samples, ncol = n_channels)

  for (r in 1:n_channels) {
    selected_atoms[[r]] <- results[[r]]$selected_atoms
    reconstruction[, r] <- results[[r]]$selected_atoms %*% results[[r]]$coefs
    original_signal[, r] <- results[[r]]$signal
  }

  out <- list()
  out$atoms <- data.frame(
    channel_id,
    atom_number,
    energy,
    envelope,
    frequency,
    phase,
    scale,
    position
  )
  out$signal <- original_signal
  out$reconstruction <- reconstruction
  out$selected_atoms <- selected_atoms
  out$time <- time
  out$sampling_frequency <- sampling_frequency
  class(out) <- "mp"

  return(out)
}
