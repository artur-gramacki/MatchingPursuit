#' Select best Gabor atoms based on phase-invariant similarity
#'
#' This function constructs a sparse, signal-dependent Gabor dictionary by
#' selecting the most relevant atoms from a precomputed atom dictionary.
#'
#' In the first step, phase-invariant similarities between complex Gabor atoms
#' and the input signal are computed using cross-products. In the second step,
#' the top-ranked atoms are reconstructed with optimal phase alignment and
#' converted into real-valued time-domain signals. The resulting object is used
#' as input to \code{omp_core()}, \code{mp_core()} or to \code{mp_omp_execute()}.
#' This second function is a wrapper around the first function. It is prepared
#' in such a way that an object of class \code{mp} is created as output.
#' This allows it to be passed to the \code{tf_map()} function, which creates
#' a time-frequency map.
#'
#' @param atoms_dict A matrix describing Gabor atoms (e.g. output of
#'   \code{read_dict()}). Each row represents a candidate atom with fields:
#'   \code{block}, \code{time_sec}, \code{freq_hz}, \code{window_len}, etc.
#'
#' @param atoms_dict A matrix describing Gabor atoms (e.g. output of
#'   \code{read_dict()}). Each row represents a candidate atom and must
#'   contain the following columns: \code{block}, \code{time_sec}, \code{freq_hz},
#'   and \code{window_len}. Other columns are ignored.
#'
#' @param signal A numeric vector, matrix, or data frame representing the signal(s)
#'   to be analyzed. Each column is treated as a separate channel.
#'
#' @param sf Sampling frequency (Hz) of the signal.
#'
#' @param topk Number of best atoms to select per signal.
#'   If \code{NULL}, defaults to \code{ceiling(0.05 * nrow(atoms_dict))}.
#'
#' @param sigma_divisor Optional parameter controlling the width of the Gaussian
#'   window. Larger values produce narrower windows. If \code{NULL}, a default
#'   heuristic is used.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed during
#'   both similarity computation and atom generation.
#'
#' @return An object of class \code{"topk"}, a list containing:
#' \item{inner_products}{Matrix of phase-invariant similarities between all atoms
#'   in the dictionary and signal channels.}
#' \item{topk_indices}{Matrix of indices of the selected top-k atoms for each channel.}
#' \item{atoms}{List of matrices containing reconstructed real-valued atoms
#'   (one matrix per signal channel, where columns represent individual atoms).}
#' \item{frequency}{Matrix of frequencies (Hz) of the selected atoms for each channel.}
#' \item{phase}{Matrix of optimal phase values used for atom reconstruction.}
#' \item{scale}{Matrix of Gaussian window scales (normalized sigma in seconds) for each atom.}
#' \item{position}{Matrix of time positions (centers of atoms in seconds) for each atom.}
#' \item{atom_begin}{Matrix of start times of each atom (in seconds).}
#' \item{window_len}{Matrix of window lengths (in seconds).}
#'
#' @export
#'
#' @seealso
#' \code{\link{read_dict}},
#' \code{\link{mp_omp_execute}}
#' \code{\link{omp_core}}
#' \code{\link{mp_core}}
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
#' sample3 <- read_csv_signals(
#'   sig_file,
#'   col_names_in_csv = TRUE
#' )
#'
#' sf <- sample3$sampling_frequency
#' signal <- sample3$signal
#' duration <- nrow(sample3$signal) / sf
#'
#' # +-------------------------------------------------------------+
#' # | Step 2: Read dictionary                                     |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample3_dict.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' atoms_dict <- read_dict(
#'   xml_file,
#'   sf,
#'   duration,
#'   verbose = TRUE
#' )
#'
#' head(atoms_dict)
#' tail(atoms_dict)
#' nrow(atoms_dict)
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Select top-k atoms most similar to the signal       |
#' # +-------------------------------------------------------------+
#' out_topk_atoms <- topk_atoms(
#'   atoms_dict = atoms_dict,
#'   signal = signal,
#'   sigma_divisor = NULL,
#'   sf = sf,
#'   topk = 5000,
#'   verbose = TRUE
#' )
#'
#' class(out_topk_atoms)
#'
#' # +-------------------------------------------------------------+
#' # | Step 4.1                                                    |
#' # | Apply OMP to obtain a sparse representation of the signal   |
#' # +-------------------------------------------------------------+
#' # | Output: object of class 'mp'                                |
#' # | Processes: all signal channels (3 in this example)          |
#' # +-------------------------------------------------------------+
#' fit_1 <- mp_omp_execute(
#'   mode = "omp",
#'   dictionary = out_topk_atoms,
#'   signal = signal,
#'   sf = sf,
#'   n_nonzero_coefs = 50
#' )
#'
#' class(fit_1)
#'
#' # +-------------------------------------------------------------+
#' # | Step 4.2                                                    |
#' # | Apply OMP to obtain a sparse representation of the signal   |
#' # +-------------------------------------------------------------+
#' # | Output: list with atom parameters                           |
#' # | Processes: one selected channel                             |
#' # +-------------------------------------------------------------+
#' fit_2 <- omp_core(
#'   dictionary = out_topk_atoms,
#'   signal = signal,
#'   channel = 1,
#'   n_nonzero_coefs = 50
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 5: Plot time-frequency representation                  |
#' # +-------------------------------------------------------------+
#' plot(fit_1, channel = 3)
#'
topk_atoms <- function(atoms_dict, signal, sf, topk = NULL, sigma_divisor = NULL, verbose = FALSE) {

  if (!is.matrix(signal)) {
    if (is.vector(signal) || is.data.frame(signal)) {
      signal <- as.matrix(signal)
    } else {
      stop("Parameter must be a matrix or convertible to a matrix")
    }
  }

  proj_mod_mtx <- matrix(0, nrow = nrow(atoms_dict), ncol = ncol(signal))
  N <- nrow(signal)

  # By default select 5% best atoms
  if (is.null(topk)) {
    topk <- ceiling(0.05 * nrow(atoms_dict))
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

  if (verbose) message("topk_atoms(), step 1, calculating ", nrow(atoms_dict), " inner products...")

  blocks_id <- unique(atoms_dict[,"block"])

  for (i in blocks_id) {
    ids <- which(atoms_dict[, "block"] == i)
    block <- atoms_dict[ids,]
    my_list <- gabor_proj_fft(block, signal)
    proj_mod_mtx[ids,] <- my_list$proj_mod_mtx
  }

  if (verbose) message("topk_atoms(), step 1 finished.")

  # ------------------------------------------------------------------+
  # STEP 2 ----
  # Generate top-k atoms with optimal phase
  #                           ^^^^^^^^^^^^^
  # ------------------------------------------------------------------+
  atoms_list <- list()

  for (s in 1:ncol(signal)) {
    atoms_list[[s]] <- matrix(NA, nrow = N, ncol = topk)
  }
  names(atoms_list) <- paste0("signal_", 1:ncol(signal))

  times_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))
  times_center_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))
  freq_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))
  sigma_mtx <- matrix(NA,  nrow = topk, ncol = ncol(signal))
  window_len_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))
  topk_idx_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))
  phase_mtx <- matrix(NA, nrow = topk, ncol = ncol(signal))

  for (i in 1:ncol(signal)) {
    topk_idx <- order(proj_mod_mtx[, i], decreasing = TRUE)[1:topk]
    topk_atoms_dict <- atoms_dict[topk_idx,]

    atoms_mtx <- matrix(NA, nrow = N, ncol = topk)
    times_vec <- numeric(topk)
    times_center_vec <- numeric(topk)
    freq_vec <- numeric(topk)
    sigma_vec <- numeric(topk)
    window_len_vec <- numeric(topk)
    phase_vec <- numeric(topk)

    blocks_id <- unique(topk_atoms_dict[,"block"])
    topk_proj_mod_mtx <- matrix(0, nrow = nrow(topk_atoms_dict), ncol = ncol(signal))
    topk_fft_bin_mtx <- matrix(0, nrow = nrow(topk_atoms_dict), ncol = ncol(signal))

    for (k in blocks_id) {
      ids <- which(topk_atoms_dict[, "block"] == k)
      block <- topk_atoms_dict[ids, , drop = FALSE]
      my_list <- gabor_proj_fft(block, signal)
      topk_proj_mod_mtx[ids,] <- my_list$proj_mod_mtx
      topk_fft_bin_mtx[ids,] <- my_list$fft_bin_mtx
    }

    # optimal phis
    phi_vec <- Arg(as.vector(topk_fft_bin_mtx[, i]))

    for (j in 1:nrow(topk_atoms_dict)) {

      time <- topk_atoms_dict[j, "time_sec"]
      freq <- topk_atoms_dict[j, "freq_hz"]
      window_len <- topk_atoms_dict[j, "window_len"]

      n0 <- as.integer(time * sf) + 1
      n <- 0:(window_len - 1)
      c <- (window_len - 1) / 2

      if (is.null(sigma_divisor)) {
        sigma <- (window_len + 1) / 3
      } else {
        sigma <- (window_len + 1) / sigma_divisor
      }

      w <- exp(-pi * ((n - c) / sigma)^2)
      end_idx <- min(n0 + window_len - 1, N)
      valid_len <- end_idx - n0 + 1
      phi <- phi_vec[j]

      # ------------------------------------------------------------------+
      # Final real-valued atom with optimal phi ----
      #       ^^^^^^^^^^^^^^^^      ^^^^^^^^^^^
      # ------------------------------------------------------------------+
      carrier <- cos(2 * pi * freq * n / sf + phi)
      atom <- w * carrier
      x <- rep(0, N)
      x[n0:end_idx] <- atom[1:valid_len]

      # Normalize final atom
      norm <- sqrt(sum(x^2))

      if (norm > 0) {
        x <- x / norm
      }

      atoms_mtx[, j] <- x
      times_vec[j] <- time
      times_center_vec[j] <- (time + (window_len / (2 * sf)))
      freq_vec[j] <- freq
      sigma_vec[j] <- sigma / sf
      window_len_vec[j] <- window_len / sf
      phase_vec[j] <- phi

    } ### for (j in topk_idx)

    if (verbose) message("topk_atoms(), step 2, signal ", i, " finished.")

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
