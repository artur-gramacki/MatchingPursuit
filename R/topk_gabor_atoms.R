#' Select the most relevant Gabor atoms using phase-invariant similarity
#'
#' Selects a signal-dependent subset of Gabor atoms with the largest
#' phase-invariant projections onto the input signal. The selected atoms are
#' represented by their parameters; full time-domain atom waveforms are not
#' constructed or stored.
#'
#' Complex projection coefficients are computed for candidate atoms block by
#' block using \code{gabor_projection_fft()}. Only the current top-ranked
#' projection values, corresponding atom indices, and complex coefficients are
#' retained. The complex coefficients are subsequently used to determine the
#' optimal phase of each selected atom.
#'
#' @param atoms_dict A matrix describing candidate Gabor atoms, typically
#'   returned by \code{read_gabor_dict()}. Each row represents one candidate
#'   atom. The matrix must contain at least the columns \code{block},
#'   \code{time_sample}, \code{time_sec}, \code{freq_bin}, \code{freq_hz},
#'   \code{window_len}, and \code{fft_size}.
#'
#' @param signal An object of class \code{"sig"}, \code{"edf"}, or
#'   \code{"wfdb"} containing the signal(s) to be analyzed. Each signal column
#'   is treated as a separate channel.
#'
#' @param topk Positive integer specifying the number of highest-ranked atoms
#'   retained for each signal channel. If \code{NULL}, the number is set to
#'   \code{ceiling(0.1 * nrow(atoms_dict))}.
#'
#' @param sigma_divisor Optional positive numeric value controlling the width
#'   of the Gaussian envelope. The envelope scale is calculated as
#'   \code{(window_len + 1) / sigma_divisor}. Larger values produce narrower
#'   envelopes. If \code{NULL}, a divisor of \code{3} is used.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed
#'   during projection computation and parameter storage.
#'
#' @details
#' Candidate atoms are ranked separately for each signal channel according to
#' the magnitudes of their complex projection coefficients. Projection values
#' are processed block by block, and only the current top-k values,
#' corresponding atom indices, and complex coefficients are retained. This
#' avoids storing the complete matrix of projection values for all candidate
#' atoms.
#'
#' The ranking is independent of the phase of the corresponding real-valued
#' Gabor atom. The optimal phase of each selected atom is obtained from its
#' retained complex coefficient and stored together with the remaining Gabor
#' parameters.
#'
#' Full time-domain atom waveforms are not materialized by this function. The
#' returned object stores the parameters required to reconstruct the selected
#' atoms later using \code{gabor_atoms_matrix()}, including their zero-based
#' start positions and support lengths in samples, the sampling frequency,
#' signal length, and the Gaussian-envelope divisor actually used.
#'
#' Atom supports may extend beyond the observed signal boundaries when the
#' input dictionary was generated with \code{full_atoms_in_signal = FALSE}
#' in \code{read_gabor_dict()}. In this case, \code{atom_begin_sample} may be
#' negative. Retaining this value allows the correct fragment of a
#' boundary-crossing atom to be reconstructed later.
#'
#' @return An object of class \code{"topk"}, containing:
#' \item{n_candidates}{
#'   Number of candidate atoms considered during the selection step.
#' }
#' \item{topk_inner_products}{
#'   Matrix of phase-invariant projection magnitudes for the selected top-k
#'   atoms. Rows correspond to selected atoms and columns to signal channels.
#' }
#' \item{topk_indices}{
#'   Integer matrix containing the row indices in \code{atoms_dict} of the
#'   selected top-k atoms for each signal channel.
#' }
#' \item{frequency}{
#'   Matrix of frequencies, in Hz, of the selected atoms.
#' }
#' \item{phase}{
#'   Matrix of optimal phases, in radians, of the selected atoms.
#' }
#' \item{scale}{
#'   Matrix of Gaussian envelope scales (sigma), in seconds.
#' }
#' \item{position}{
#'   Matrix of center positions of the selected atoms, in seconds.
#' }
#' \item{atom_begin}{
#'   Matrix of start positions of the complete atom supports, in seconds.
#'   Values may be negative when boundary-crossing atoms are allowed.
#' }
#' \item{window_len}{
#'   Matrix of complete atom support lengths, in seconds.
#' }
#' \item{atom_begin_sample}{
#'   Integer matrix of zero-based start positions of the complete atom
#'   supports, in samples. Values may be negative for boundary-crossing atoms.
#' }
#' \item{window_len_samples}{
#'   Integer matrix of complete atom support lengths, in samples.
#' }
#' \item{sampling_frequency}{
#'   Sampling frequency of the analyzed signal, in Hz.
#' }
#' \item{signal_length}{
#'   Number of samples in the analyzed signal.
#' }
#' \item{sigma_divisor}{
#'   Gaussian-envelope divisor used to calculate sigma. This is the supplied
#'   \code{sigma_divisor}, or \code{3} when \code{sigma_divisor = NULL}.
#' }
#'
#' @export
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{gabor_projection_fft}},
#' \code{\link{gabor_atoms_matrix}},
#' \code{\link{mp_core}},
#' \code{\link{omp_core}},
#' \code{\link{mp_omp_execute}}
#'
#' @examples
#' # +-------------------------------------------------------------+
#' # | Step 1: Read signal                                         |
#' # +-------------------------------------------------------------+
#' sig_file <- system.file(
#'   "extdata",
#'   "sample3.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' signal <- read_csv_signals(
#'   sig_file,
#'   col_names_in_csv = TRUE
#' )
#'
#' sampling_frequency <- signal$sampling_frequency
#' duration <- nrow(signal$signal) / sampling_frequency
#'
#' # +-------------------------------------------------------------+
#' # | Step 2: Read dictionary                                     |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample3.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' atoms_dict <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = sampling_frequency,
#'   duration = duration,
#'   verbose = TRUE,
#'   full_atoms_in_signal = FALSE
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Select top-k atoms most similar to the signal       |
#' # +-------------------------------------------------------------+
#' out_topk <- topk_gabor_atoms(
#'   atoms_dict = atoms_dict,
#'   signal = signal,
#'   topk = 5000,
#'   verbose = TRUE
#' )
#'
#' class(out_topk)
#' summary(out_topk)
#'
#' # Selected atoms can subsequently be materialized as a matrix:
#' D_sparse <- gabor_atoms_matrix(out_topk, sparse = TRUE)
#'
topk_gabor_atoms <- function(atoms_dict, signal, topk = NULL, sigma_divisor = NULL, verbose = FALSE) {

  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig <- as.matrix(signal$signal)
  sampling_frequency <- signal$sampling_frequency

  if (!is.null(sigma_divisor)) {
    if (length(sigma_divisor) != 1L ||
        !is.finite(sigma_divisor) ||
        sigma_divisor <= 0) {
      stop("'sigma_divisor' must be a positive finite number.")
    }
  }

  N <- nrow(sig)

  # By default select 10% best atoms
  if (is.null(topk)) {
    topk <- ceiling(0.1 * nrow(atoms_dict))
  }

  if (length(topk) != 1L ||
      !is.finite(topk) ||
      topk < 1L ||
      topk != as.integer(topk)) {
    stop("'topk' must be a positive integer.")
  }

  if (topk > nrow(atoms_dict)) {
    stop("'topk' cannot be greater than ", nrow(atoms_dict), ".")
  }

  # ------------------------------------------------------------------+
  # STEP 1 ----
  # Calculate phase-invariant similarities
  # using complex Gabor atoms
  # ------------------------------------------------------------------+
  #
  # In this step, we don't save all the generated atoms or all projection
  # values. Projections are calculated block by block, and only the current
  # top-k values, corresponding atom indices, and complex coefficients are
  # retained.

  if (verbose) message("topk_gabor_atoms(), step 1, calculating ", nrow(atoms_dict), " inner products...")

  best_values <- matrix(-Inf, nrow = topk, ncol = ncol(sig))
  best_indices <- matrix(NA_integer_, nrow = topk, ncol = ncol(sig))
  best_fft <- matrix(NA_complex_, nrow = topk, ncol = ncol(sig))

  blocks_id <- unique(atoms_dict[,"block"])

  for (i in blocks_id) {
    ids <- which(atoms_dict[, "block"] == i)
    block <- atoms_dict[ids, , drop = FALSE]
    my_list <- gabor_projection_fft(block, sig, sigma_divisor = sigma_divisor)

    for (s in seq_len(ncol(sig))) {
      values <- c(best_values[, s], my_list$proj_mod_mtx[, s])
      indices <- c(best_indices[, s], ids)
      fft_values <- c(best_fft[, s], my_list$fft_bin_mtx[, s])

      keep <- order(values, decreasing = TRUE)[seq_len(topk)]

      best_values[, s] <- values[keep]
      best_indices[, s] <- indices[keep]
      best_fft[, s] <- fft_values[keep]
    }
  }

  if (verbose) message("topk_gabor_atoms(): step 1 finished.")

  # ------------------------------------------------------------------+
  # STEP 2 ----
  # Store parameters of the selected top-k atoms
  # ------------------------------------------------------------------+
  times_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  times_center_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  freq_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  sigma_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  window_len_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  topk_idx_mtx <- matrix(NA_integer_, nrow = topk, ncol = ncol(sig))
  phase_mtx <- matrix(NA_real_, nrow = topk, ncol = ncol(sig))
  atom_begin_sample_mtx <- matrix(NA_integer_, nrow = topk, ncol = ncol(sig))
  window_len_samples_mtx <- matrix(NA_integer_, nrow = topk, ncol = ncol(sig))

  sigma_div <- if (is.null(sigma_divisor)) 3 else sigma_divisor

  for (i in 1:ncol(sig)) {
    topk_idx <- best_indices[, i]
    topk_atoms_dict <- atoms_dict[topk_idx, , drop = FALSE]

    times_vec <- numeric(topk)
    times_center_vec <- numeric(topk)
    freq_vec <- numeric(topk)
    sigma_vec <- numeric(topk)
    window_len_vec <- numeric(topk)
    phase_vec <- numeric(topk)
    atom_begin_sample_vec <- integer(topk)
    window_len_samples_vec <- integer(topk)

    # Optimal phases of the selected atoms
    phi_vec <- Arg(best_fft[, i])

    for (j in 1:nrow(topk_atoms_dict)) {

      time <- topk_atoms_dict[j, "time_sec"]
      t_sample <- as.integer(topk_atoms_dict[j, "time_sample"])
      freq <- topk_atoms_dict[j, "freq_hz"]
      window_len <- as.integer(topk_atoms_dict[j, "window_len"])
      sigma <- (window_len + 1) / sigma_div

      phi <- phi_vec[j]

      times_vec[j] <- time
      times_center_vec[j] <- time + ((window_len - 1) / (2 * sampling_frequency))
      freq_vec[j] <- freq
      sigma_vec[j] <- sigma / sampling_frequency
      window_len_vec[j] <- window_len / sampling_frequency
      phase_vec[j] <- phi
      atom_begin_sample_vec[j] <- t_sample
      window_len_samples_vec[j] <- window_len

    } ### for (j in 1:nrow(topk_atoms_dict))

    if (verbose) {
      if (ncol(sig) == 1) {
        message("topk_gabor_atoms(): step 2 finished.")
      } else{
        message("topk_gabor_atoms(): step 2, signal ", i, " finished.")
      }
      message("topk_gabor_atoms(): ", topk, " out of ",  nrow(atoms_dict), " atoms selected successfully.")
    }

    times_mtx[, i] <- times_vec
    times_center_mtx[, i] <- times_center_vec
    freq_mtx[, i] <- freq_vec
    phase_mtx[, i] <- phase_vec
    sigma_mtx[, i] <- sigma_vec
    window_len_mtx[, i] <- window_len_vec
    topk_idx_mtx[, i] <- topk_idx
    atom_begin_sample_mtx[, i] <- atom_begin_sample_vec
    window_len_samples_mtx[, i] <- window_len_samples_vec

  } ###  for (i in 1:ncol(sig))

  output <- list(
    n_candidates = nrow(atoms_dict),
    topk_inner_products = best_values,
    topk_indices = topk_idx_mtx,
    #atoms = atoms_list,
    frequency = freq_mtx,
    phase = phase_mtx,
    scale = sigma_mtx,
    position = times_center_mtx,
    atom_begin = times_mtx,
    window_len = window_len_mtx,
    atom_begin_sample = atom_begin_sample_mtx,
    window_len_samples = window_len_samples_mtx,
    sampling_frequency = sampling_frequency,
    signal_length = N,
    sigma_divisor = sigma_div
  )

  class(output) <- "topk"

  return(output)
}
