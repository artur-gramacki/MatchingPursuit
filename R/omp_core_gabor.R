# Update the Cholesky factor after adding one unit-L2-normalized atom.
update_gabor_omp_cholesky <- function(active_atoms, new_atom, L) {

  if (ncol(active_atoms) == 0L) {
    return(list(ok = TRUE, L = matrix(1, nrow = 1L, ncol = 1L), alpha = 1))
  }

  w <- as.numeric(crossprod(active_atoms, new_atom))
  v <- forwardsolve(L, w)
  alpha <- 1 - sum(v^2)

  if (!is.finite(alpha) || alpha <= 1e-12) {
    return(list(ok = FALSE, L = L, alpha = alpha))
  }

  L_new <- rbind(
    cbind(L, rep(0, nrow(L))),
    c(v, sqrt(alpha))
  )

  list(ok = TRUE, L = L_new, alpha = alpha)
}

#' Orthogonal Matching Pursuit for a Parameterized Gabor Dictionary
#'
#' Performs Orthogonal Matching Pursuit directly on a parameterized Gabor
#' dictionary without materializing the complete dictionary as a time-domain
#' matrix.
#'
#' @param atoms_dict Matrix-like object describing candidate Gabor atoms,
#'   typically returned by \code{read_gabor_dict()}. It must contain the columns
#'   \code{block}, \code{time_sample}, \code{time_sec}, \code{freq_bin},
#'   \code{freq_hz}, \code{window_len}, and \code{fft_size}.
#'
#' @param signal An object of class \code{"sig"}, \code{"edf"}, or \code{"wfdb"}.
#'
#' @param channel Integer index of the signal channel to analyze. If \code{NULL},
#'   channel 1 is used for a single-channel signal; for a multichannel signal,
#'   the channel must be specified explicitly.
#'
#' @param n_nonzero_coefs Optional positive integer specifying the maximum
#'   number of selected atoms when \code{tol = NULL}. If both
#'   \code{n_nonzero_coefs} and \code{tol} are \code{NULL},
#'   a default of 50 atoms is used.
#'
#' @param tol Optional stopping tolerance for the relative residual energy.
#'   If specified, it overrides \code{n_nonzero_coefs}.
#'
#' @param sigma_divisor Optional positive numeric value controlling the width
#'   of the Gaussian envelope. If \code{NULL}, a value of 3 is used.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed.
#'
#' @details
#' Candidate selection uses the same parameterized Gabor search as
#' \code{mp_core_gabor()}. At each OMP iteration, all candidates are evaluated
#' against the current residual and the phase of the newly selected atom is
#' optimized analytically.
#'
#' Once an atom has been selected, its phase remains fixed. The coefficients
#' of all selected atoms are then recomputed jointly by least squares, as in
#' classical OMP. The least-squares problem is updated efficiently using an
#' incremental Cholesky factorization.
#'
#' The complete Gabor dictionary is never stored as a time-domain matrix.
#' Only the selected unit-L2-normalized atoms are materialized and retained in
#' the active dictionary.
#'
#' A single row of \code{atoms_dict} represents a fixed time position, frequency,
#' and window length, while phase is optimized continuously. Consequently, the
#' same row index may in principle be selected more than once with different
#' phases, provided that the resulting time-domain atoms are linearly
#' independent. Near-linear dependence is detected through the Cholesky update;
#' a rejected candidate is skipped and the next-best candidate is tested.
#'
#' @return A list containing:
#'
#' \item{selected_atoms}{Matrix whose columns are the selected
#'   unit-L2-normalized time-domain atoms.}
#' \item{signal}{Analyzed signal channel as a numeric vector.}
#' \item{reconstruction}{OMP reconstruction of the signal.}
#' \item{coefs}{Final least-squares coefficients corresponding to
#'   \code{selected_atoms}.}
#' \item{energy}{Reduction in residual energy produced at each OMP iteration.}
#' \item{support}{Row indices of the selected atoms in \code{atoms_dict}, in
#'   selection order.}
#' \item{residual}{Final residual signal.}
#' \item{relative_residual_energy}{Relative residual energy at iteration 0 and
#'   after each completed OMP iteration.}
#' \item{n_iters}{Number of completed OMP iterations.}
#' \item{frequency}{Selected atom frequencies in Hz.}
#' \item{phase}{Optimized phases fixed at the time each atom was selected.}
#' \item{scale}{Selected atom scales in seconds.}
#' \item{position}{Selected atom centre positions in seconds.}
#' \item{atom_begin}{Selected atom start positions in seconds.}
#' \item{window_len}{Selected atom window lengths in seconds.}
#' \item{atom_begin_sample}{Selected atom start positions in samples.}
#' \item{window_len_samples}{Selected atom window lengths in samples.}
#' \item{sampling_frequency}{Sampling frequency in Hz.}
#' \item{sigma_divisor}{Gaussian envelope divisor actually used.}
#' \item{n_candidates}{Number of parameter rows in \code{atoms_dict}.}
#'
#' @seealso
#' \code{\link{mp_core_gabor}},
#' \code{\link{read_gabor_dict}},
#' \code{\link{gabor_projection_fft}},
#' \code{\link{omp_core}}
#'
#' @export
#'
#' @examples
#' signal_file <- system.file(
#'   "extdata",
#'   "sample1.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' xml_file <- system.file(
#'   "extdata",
#'   "one_block.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' signal <- read_csv_signals(signal_file, col_names_in_csv = FALSE)
#'
#' atoms_dict <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = signal$sampling_frequency,
#'   duration = nrow(signal$signal) / signal$sampling_frequency,
#'   full_atoms_in_signal = TRUE
#' )
#'
#' fit <- omp_core_gabor(
#'   atoms_dict = atoms_dict,
#'   signal = signal,
#'   channel = 1,
#'   n_nonzero_coefs = 10
#' )
omp_core_gabor <- function(
    atoms_dict,
    signal,
    channel = NULL,
    n_nonzero_coefs = NULL,
    tol = NULL,
    sigma_divisor = NULL,
    verbose = FALSE
) {

  # Input signal ----
  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig_mtx <- as.matrix(signal$signal)
  sampling_frequency <- signal$sampling_frequency

  if (is.null(channel)) {
    if (ncol(sig_mtx) == 1L) channel <- 1L
    else stop("'channel' must be specified for multichannel signals.")
  }

  if (length(channel) != 1L || !is.finite(channel) || channel < 1L ||
      channel > ncol(sig_mtx) || channel != as.integer(channel)) {
    stop("'channel' is out of range.")
  }

  sig <- as.numeric(sig_mtx[, channel])
  N <- length(sig)

  if (any(!is.finite(sig))) stop("Signal contains non-finite values.")

  total_energy <- sum(sig^2)
  if (total_energy < 1e-12) stop("Signal has zero energy.")


  # Candidate dictionary ----
  required <- c(
    "block", "time_sample", "time_sec", "freq_bin",
    "freq_hz", "window_len", "fft_size"
  )

  missing <- setdiff(required, colnames(atoms_dict))
  if (length(missing) > 0L) {
    stop("'atoms_dict' does not contain required columns: ",
         paste(missing, collapse = ", "), ".")
  }

  p <- nrow(atoms_dict)
  if (p == 0L) stop("'atoms_dict' contains no candidate atoms.")

  if (!is.null(n_nonzero_coefs)) {
    if (length(n_nonzero_coefs) != 1L || !is.finite(n_nonzero_coefs) ||
        n_nonzero_coefs < 1L || n_nonzero_coefs != as.integer(n_nonzero_coefs)) {
      stop("'n_nonzero_coefs' must be a positive integer.")
    }
  }

  if (!is.null(tol)) {
    if (length(tol) != 1L || !is.finite(tol) || tol < 0 || tol > 1) {
      stop("'tol' must be a finite number between 0 and 1.")
    }
  }

  if (!is.null(sigma_divisor)) {
    if (length(sigma_divisor) != 1L || !is.finite(sigma_divisor) ||
        sigma_divisor <= 0) {
      stop("'sigma_divisor' must be a positive finite number.")
    }
  }

  sigma_div <- if (is.null(sigma_divisor)) 3 else sigma_divisor

  # Each parameter row spans at most a two-dimensional cosine-sine subspace.
  # The number of linearly independent selected atoms also cannot exceed N.
  max_possible <- as.integer(min(N, 2 * as.double(p)))

  if (is.null(tol)) {
    if (is.null(n_nonzero_coefs)) n_nonzero_coefs <- 50L
    max_iter <- min(as.integer(n_nonzero_coefs), max_possible)
  } else {
    max_iter <- max_possible
  }


  # Dictionary preparation ----
  prep <- prepare_gabor_dictionary(
    atoms_dict = atoms_dict,
    signal_length = N,
    sampling_frequency = sampling_frequency,
    sigma_divisor = sigma_divisor
  )


  # Output buffers and active OMP system ----
  support <- integer(max_iter)

  frequency <- numeric(max_iter)
  phase <- numeric(max_iter)
  scale <- numeric(max_iter)
  position <- numeric(max_iter)
  atom_begin <- numeric(max_iter)
  window_len_out <- numeric(max_iter)
  atom_begin_sample <- integer(max_iter)
  window_len_samples <- integer(max_iter)

  active_atoms <- matrix(0, nrow = N, ncol = 0L)
  b <- numeric(0)
  L <- NULL
  x_active <- numeric(0)

  residual <- sig

  residual_energy <- numeric(max_iter + 1L)
  residual_energy[1L] <- total_energy

  relative_residual_energy <- numeric(max_iter + 1L)
  relative_residual_energy[1L] <- 1

  n_iters <- 0L


  # Orthogonal Matching Pursuit ----
  for (k in seq_len(max_iter)) {

    # 1. Search the full parameterized dictionary and optimize the phase of
    #    the new atom with respect to the current OMP residual. If the best
    #    candidate is nearly linearly dependent on the active set, reject that
    #    parameter row for this iteration and try the next-best candidate.
    rejected <- integer(0)
    selected <- FALSE

    repeat {

      best <- find_best_gabor_atom(
        atoms_dict = atoms_dict,
        residual = residual,
        prep = prep,
        exclude = rejected
      )

      if (is.null(best)) break

      best_atom_idx <- best$atom_idx
      best_phase <- best$phase

      # 2. Construct only the candidate time-domain atom. Its phase is fixed
      #    if the atom is accepted into the active OMP set.
      winner <- build_gabor_atom(
        atoms_dict = atoms_dict,
        atom_idx = best_atom_idx,
        phase = best_phase,
        prep = prep
      )

      atom <- winner$atom

      # 3. Check whether the candidate can be added to the active Gram matrix.
      chol <- update_gabor_omp_cholesky(
        active_atoms = active_atoms,
        new_atom = atom,
        L = L
      )

      if (chol$ok) {
        selected <- TRUE
        break
      }

      rejected <- c(rejected, best_atom_idx)

      if (verbose) {
        message(
          "iteration: ", k,
          ", rejected near-dependent atom: ", best_atom_idx
        )
      }
    }

    if (!selected) {
      warning(paste(
        "No linearly independent Gabor candidate found at iteration",
        k
      ))
      break
    }

    L <- chol$L

    row <- winner$row
    t_sample <- winner$t_sample
    freq <- winner$freq
    window_len <- winner$window_len

    # 4. Add the atom to the active dictionary and store its fixed parameters.
    active_atoms <- cbind(active_atoms, atom)

    support[k] <- best_atom_idx
    frequency[k] <- freq
    phase[k] <- best_phase
    atom_begin[k] <- row[1L, "time_sec"]
    atom_begin_sample[k] <- t_sample
    window_len_out[k] <- window_len / sampling_frequency
    window_len_samples[k] <- window_len

    position[k] <- row[1L, "time_sec"] +
      (window_len - 1) / (2 * sampling_frequency)

    scale[k] <- ((window_len + 1) / sigma_div) / sampling_frequency

    # 5. Re-estimate all active coefficients by least squares.
    #    b = A' * signal is extended only by the newly selected atom.
    b <- c(b, sum(atom * sig))

    z <- forwardsolve(L, b)
    x_active <- as.numeric(backsolve(t(L), z))

    # 6. Orthogonal residual update.
    residual <- sig - as.numeric(active_atoms %*% x_active)

    residual_energy[k + 1L] <- sum(residual^2)
    relative_residual_energy[k + 1L] <-
      residual_energy[k + 1L] / total_energy

    n_iters <- k

    if (verbose) {
      explained <- 100 * (1 - relative_residual_energy[k + 1L])

      message("iteration: ", k,
              ", selected atom: ", best_atom_idx,
              ", coefficient: ", signif(x_active[k], 6),
              ", relative residual energy: ",
              signif(relative_residual_energy[k + 1L], 8),
              ", explained energy: ", signif(explained, 6), "%")
    }

    if (!is.null(tol) &&
        relative_residual_energy[k + 1L] <= tol) {
      if (verbose) message("Convergence achieved in iteration: ", k)
      break
    }

    if (residual_energy[k + 1L] < 1e-12 * total_energy) {
      if (verbose) message("Residual energy is practically zero.")
      break
    }
  }


  # Trim output buffers ----
  ii <- seq_len(n_iters)

  support <- support[ii]

  frequency <- frequency[ii]
  phase <- phase[ii]
  scale <- scale[ii]
  position <- position[ii]
  atom_begin <- atom_begin[ii]
  window_len_out <- window_len_out[ii]
  atom_begin_sample <- atom_begin_sample[ii]
  window_len_samples <- window_len_samples[ii]

  selected_atoms <- active_atoms[, ii, drop = FALSE]
  coefs <- x_active[ii]

  residual_energy <- residual_energy[seq_len(n_iters + 1L)]
  relative_residual_energy <-
    relative_residual_energy[seq_len(n_iters + 1L)]

  energy <- residual_energy[ii] - residual_energy[ii + 1L]


  # Output ----
  list(
    selected_atoms = selected_atoms,
    signal = sig,
    reconstruction = as.vector(sig - residual),
    coefs = coefs,
    energy = energy,
    support = support,
    residual = as.vector(residual),
    relative_residual_energy = relative_residual_energy,
    n_iters = n_iters,
    frequency = frequency,
    phase = phase,
    scale = scale,
    position = position,
    atom_begin = atom_begin,
    window_len = window_len_out,
    atom_begin_sample = atom_begin_sample,
    window_len_samples = window_len_samples,
    sampling_frequency = sampling_frequency,
    sigma_divisor = sigma_div,
    n_candidates = p
  )
}
