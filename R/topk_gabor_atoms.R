#' Select the most relevant Gabor atoms using phase-invariant similarity
#'
#' Constructs a sparse, signal-dependent Gabor dictionary by selecting the
#' atoms with the largest phase-invariant projections onto the input signal.
#'
#' In the first step, complex projection coefficients are computed for all
#' candidate atoms using \code{gabor_projection_fft()}, and atoms are ranked
#' according to the magnitudes of these coefficients. In the second step, the
#' top-ranked atoms are reconstructed as real-valued time-domain atoms using
#' their optimal phases.
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
#'   \code{ceiling(0.05 * nrow(atoms_dict))}.
#'
#' @param sigma_divisor Optional positive numeric value controlling the width
#'   of the Gaussian envelope. The envelope scale is calculated as
#'   \code{(window_len + 1) / sigma_divisor}. Larger values produce narrower
#'   envelopes. If \code{NULL}, a divisor of \code{3} is used.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed
#'   during projection computation and atom reconstruction.
#'
#' @details
#' Candidate atoms are ranked separately for each signal channel according to
#' the magnitudes of their complex projection coefficients. This makes the
#' ranking independent of the phase of the corresponding real-valued Gabor
#' atom. The phase of each selected atom is subsequently obtained from the
#' complex coefficient and used to construct its real-valued representation.
#'
#' Atom supports may extend beyond the observed signal boundaries when the
#' input dictionary was generated with \code{full_atoms_in_signal = FALSE}
#' in \code{read_gabor_dict()}. In this case, signal values outside the
#' observed interval are implicitly treated as zero.
#'
#' The complete real-valued Gabor atom is normalized before boundary
#' truncation. If only part of an atom overlaps the observed signal, the
#' retained fragment is not renormalized. Consequently, boundary-crossing
#' atoms stored in the returned object may have an L2 norm smaller than one,
#' whereas atoms whose complete support lies within the signal have unit
#' L2 norm.
#'
#' The returned \code{"topk"} object contains the channel-specific atom matrices
#' that can be passed to \code{mp_core()} or \code{omp_core()} for sparse
#' signal decomposition.
#'
#' @return An object of class \code{"topk"}, containing:
#' \item{inner_products}{
#'   Matrix of phase-invariant projection magnitudes for all candidate atoms.
#'   Rows correspond to atoms in \code{atoms_dict} and columns to signal
#'   channels.
#' }
#' \item{topk_indices}{
#'   Matrix containing the row indices in \code{atoms_dict} of the selected
#'   top-k atoms for each signal channel.
#' }
#' \item{atoms}{
#'   List of matrices containing the selected real-valued Gabor atoms.
#'   Each list element corresponds to one signal channel; columns represent
#'   individual atoms and rows correspond to signal samples.
#' }
#' \item{frequency}{
#'   Matrix of frequencies, in Hz, of the selected atoms.
#' }
#' \item{phase}{
#'   Matrix of optimal phases, in radians, used to construct the selected
#'   real-valued atoms.
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
#'   Matrix of atom support lengths, in seconds.
#' }
#'
#' @export
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{gabor_projection_fft}},
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
#'   verbose = FALSE,
#'   full_atoms_in_signal = TRUE
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Select top-k atoms most similar to the signal       |
#' # +-------------------------------------------------------------+
#' out_topk <- topk_gabor_atoms(
#'   atoms_dict = atoms_dict,
#'   signal = signal,
#'   topk = 500,
#'   verbose = TRUE
#' )
#'
#' class(out_topk)
#' dim(out_topk$atoms[[1]])
#' head(out_topk$frequency[, 1])
#'
topk_gabor_atoms <- function(atoms_dict, signal, topk = NULL, sigma_divisor = NULL, verbose = FALSE) {

  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig <- as.matrix(signal$signal)
  sampling_frequency <- signal$sampling_frequency

  if (!is.null(topk)) {
    if (length(topk) != 1L ||
        !is.finite(topk) ||
        topk < 1L ||
        topk != as.integer(topk)) {
      stop("'topk' must be a positive integer.")
    }
  }

  if (!is.null(sigma_divisor)) {
    if (length(sigma_divisor) != 1L ||
        !is.finite(sigma_divisor) ||
        sigma_divisor <= 0) {
      stop("'sigma_divisor' must be a positive finite number.")
    }
  }

  proj_mod_mtx <- matrix(0, nrow = nrow(atoms_dict), ncol = ncol(sig))
  N <- nrow(sig)

  # By default select 5% best atoms
  if (is.null(topk)) {
    topk <- ceiling(0.05 * nrow(atoms_dict))
  }

  if (length(topk) != 1L ||
      !is.finite(topk) ||
      topk < 1L ||
      topk != as.integer(topk)) {
    stop("'topk' must be a positive integer.")
  }

  if (topk > nrow(proj_mod_mtx)) {
    stop("'topk' cannot be greater than ", nrow(atoms_dict), ".")
  }

  # ------------------------------------------------------------------+
  # STEP 1 ----
  # Calculate phase-invariant similarities
  # using complex Gabor atoms
  # ------------------------------------------------------------------+
  #
  # In this step, we don't save all the generated atoms. With hundreds of
  # thousands of potential atoms, this would be very inefficient, especially
  # since the vast majority of these atoms won't be selected (inner product
  # too small). We only create the proj_mod_mtx matrix with the saved inner
  # product values. In step 2, a matrix with the 'topk' atoms will be created.

  if (verbose) message("topk_gabor_atoms(), step 1, calculating ", nrow(atoms_dict), " inner products...")

  blocks_id <- unique(atoms_dict[,"block"])

  for (i in blocks_id) {
    ids <- which(atoms_dict[, "block"] == i)
    block <- atoms_dict[ids,]
    my_list <- gabor_projection_fft(block, sig, sigma_divisor = sigma_divisor)
    proj_mod_mtx[ids,] <- my_list$proj_mod_mtx
  }

  if (verbose) message("topk_gabor_atoms(), step 1 finished.")

  # ------------------------------------------------------------------+
  # STEP 2 ----
  # Generate top-k atoms with optimal phase
  #                           ^^^^^^^^^^^^^
  # ------------------------------------------------------------------+
  atoms_list <- list()

  for (s in 1:ncol(sig)) {
    atoms_list[[s]] <- matrix(NA, nrow = N, ncol = topk)
  }
  names(atoms_list) <- paste0("signal_", 1:ncol(sig))

  times_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  times_center_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  freq_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  sigma_mtx <- matrix(NA,  nrow = topk, ncol = ncol(sig))
  window_len_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  topk_idx_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))
  phase_mtx <- matrix(NA, nrow = topk, ncol = ncol(sig))

  for (i in 1:ncol(sig)) {
    topk_idx <- order(proj_mod_mtx[, i], decreasing = TRUE)[1:topk]
    topk_atoms_dict <- atoms_dict[topk_idx, , drop = FALSE]

    atoms_mtx <- matrix(NA, nrow = N, ncol = topk)
    times_vec <- numeric(topk)
    times_center_vec <- numeric(topk)
    freq_vec <- numeric(topk)
    sigma_vec <- numeric(topk)
    window_len_vec <- numeric(topk)
    phase_vec <- numeric(topk)

    blocks_id <- unique(topk_atoms_dict[,"block"])
    topk_proj_mod_mtx <- matrix(0, nrow = nrow(topk_atoms_dict), ncol = ncol(sig))
    topk_fft_bin_mtx <- matrix(0 + 0i, nrow = nrow(topk_atoms_dict), ncol = ncol(sig))

    for (k in blocks_id) {
      ids <- which(topk_atoms_dict[, "block"] == k)
      block <- topk_atoms_dict[ids, , drop = FALSE]
      my_list <- gabor_projection_fft(block, sig, sigma_divisor = sigma_divisor)
      topk_proj_mod_mtx[ids,] <- my_list$proj_mod_mtx
      topk_fft_bin_mtx[ids,] <- my_list$fft_bin_mtx
    }

    # optimal phis
    phi_vec <- Arg(as.vector(topk_fft_bin_mtx[, i]))

    for (j in 1:nrow(topk_atoms_dict)) {

      time <- topk_atoms_dict[j, "time_sec"]
      t_sample <- as.integer(topk_atoms_dict[j, "time_sample"])
      freq <- topk_atoms_dict[j, "freq_hz"]
      window_len <- as.integer(topk_atoms_dict[j, "window_len"])

      n <- 0:(window_len - 1)
      c <- (window_len - 1) / 2

      if (is.null(sigma_divisor)) {
        sigma <- (window_len + 1) / 3
      } else {
        sigma <- (window_len + 1) / sigma_divisor
      }

      w <- exp(-pi * ((n - c) / sigma)^2)
      phi <- phi_vec[j]

      # ------------------------------------------------------------------+
      # Final real-valued atom with optimal phi ----
      #      ^^^^^^^^^^^^^^^^      ^^^^^^^^^^^
      # ------------------------------------------------------------------+
      carrier <- cos(2 * pi * freq * n / sampling_frequency + phi)
      atom <- w * carrier

      # Normalize the complete atom before boundary truncation
      atom_norm <- sqrt(sum(atom^2))

      if (atom_norm > 0) {
        atom <- atom / atom_norm
      }


      # Zero-based signal positions occupied by the complete atom
      signal_idx <- t_sample + n

      # Part of the atom overlapping the observed signal
      inside <- signal_idx >= 0 & signal_idx < N

      # Construct an N-sample representation.
      # Samples outside the observed signal are implicitly zero.
      x <- numeric(N)

      if (any(inside)) {
        x[signal_idx[inside] + 1L] <- atom[inside]
      }

      atoms_mtx[, j] <- x
      times_vec[j] <- time
      times_center_vec[j] <- time + ((window_len - 1) / (2 * sampling_frequency))
      freq_vec[j] <- freq
      sigma_vec[j] <- sigma / sampling_frequency
      window_len_vec[j] <- window_len / sampling_frequency
      phase_vec[j] <- phi

    } ### for (j in topk_idx)

    if (verbose) {
      message("topk_gabor_atoms(), step 2, signal ", i, " finished.")
      message(topk, " out of ",  nrow(atoms_dict), " atoms selected successfully.\n")
    }

    atoms_list[[i]] <- atoms_mtx
    times_mtx[, i] <- times_vec
    times_center_mtx[, i] <- times_center_vec
    freq_mtx[, i] <- freq_vec
    phase_mtx[, i] <- phase_vec
    sigma_mtx[, i] <- sigma_vec
    window_len_mtx[, i] <- window_len_vec
    topk_idx_mtx[, i] <- topk_idx

  } ###  for (i in 1:ncol(sig))

  output <- list(
    inner_products = proj_mod_mtx,
    topk_indices = topk_idx_mtx,
    atoms = atoms_list,
    frequency = freq_mtx,
    phase = phase_mtx,
    scale = sigma_mtx,
    position = times_center_mtx,
    atom_begin = times_mtx,
    window_len = window_len_mtx
  )

  class(output) <- "topk"

  return(output)
}
