#' FFT-based computation of projections onto Gabor atoms
#'
#' Computes complex projection coefficients between one or more signals and a
#' set of Gabor atoms using FFT-based frequency-domain operations.
#'
#' For each time position defined in \code{block}, the corresponding signal
#' segment is multiplied by a normalized Gaussian envelope and transformed
#' using the Fast Fourier Transform. Only Fourier coefficients corresponding
#' to the frequencies specified in the dictionary are retained.
#'
#' Atom supports may extend beyond the observed signal boundaries. In this
#' case, samples outside the signal are treated as zero. The complete Gaussian
#' envelope is normalized before boundary truncation; the part overlapping the
#' observed signal is not renormalized.
#'
#' @param block A matrix describing a single Gabor dictionary block, typically
#'   obtained as a subset of the output of \code{read_gabor_dict()}.
#'   It must contain at least the columns \code{time_sample},
#'   \code{window_len}, \code{fft_size}, and \code{freq_bin}.
#'
#' @param signal A numeric vector, matrix, or data frame containing the
#'   signal(s) to be analyzed. For matrices and data frames, each column is
#'   treated as a separate signal channel.
#'
#' @param sigma_divisor Optional positive numeric value controlling the width
#'   of the Gaussian envelope. The envelope scale is calculated as
#'   \code{(window_len + 1) / sigma_divisor}. If \code{NULL}, a divisor of
#'   \code{3} is used.
#'
#' @details
#' The Gaussian envelope is constructed over the complete atom support and
#' normalized to unit L2 norm before it is applied to the signal. If the atom
#' support extends before the first signal sample or beyond the last signal
#' sample, only the overlapping signal samples contribute to the projection;
#' values outside the observed signal are implicitly treated as zero.
#'
#' Consequently, the visible part of a boundary-crossing atom is not
#' renormalized. This preserves the normalization of the complete atom
#' independently of its position relative to the signal boundaries.
#'
#' @note This function is primarily intended for internal use by \code{topk_gabor_atoms()},
#' but it is exported to support advanced experiments and methodological testing.
#'
#' @return A list with two matrices:
#'
#' \item{proj_mod_mtx}{
#'   Magnitudes of the complex projection coefficients. Rows correspond to
#'   atoms in \code{block} and columns to signal channels.
#' }
#'
#' \item{fft_bin_mtx}{
#'   Complex Fourier coefficients corresponding to the Gabor frequencies.
#'   Rows correspond to atoms in \code{block} and columns to signal channels.
#' }
#'
#' @importFrom stats mvfft
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{gabor_atoms_matrix}}
#'
#' @export
#'
#' @examples
#' signal <- as.matrix(rnorm(256))
#' sampling_frequency <- 256
#' duration <- 1
#'
#' xml_file <- system.file("extdata", "one_block.xml", package = "MatchingPursuit")
#'
#' block <- read_gabor_dict(
#'   xml_file = xml_file,
#'   full_atoms_in_signal = FALSE,
#'   sampling_frequency = sampling_frequency,
#'   duration = duration,
#'   verbose = TRUE
#' )
#'
#' out <- gabor_projection_fft(block, signal)
#'
#' pmm <- out$proj_mod_mtx
#' scm <- out$fft_bin_mtx
#'
#' head(scm)
#' head(pmm)
#'
#' # Projection magnitudes are the moduli of the complex coefficients
#' head(Mod(scm))
#'
gabor_projection_fft <- function(block, signal, sigma_divisor = NULL) {

  if (!is.null(sigma_divisor)) {
    if (length(sigma_divisor) != 1L ||
        !is.numeric(sigma_divisor) ||
        !is.finite(sigma_divisor) ||
        sigma_divisor <= 0) {
      stop("'sigma_divisor' must be a positive finite number.")
    }
  }

  if (nrow(block) == 0L) {
    stop("'block' must contain at least one atom.")
  }

  signal <- as.matrix(signal)

  N <- nrow(signal)
  K <- ncol(signal)

  proj_mod_mtx <- matrix(0, nrow = nrow(block), ncol = K)
  fft_bin_mtx <- matrix(0 + 0i, nrow = nrow(block), ncol = K)

  # window_len and fft_size are constant within a dictionary block,
  # so their values can be taken from the first row.
  window_len <- block[1L, "window_len"]
  fft_size   <- block[1L, "fft_size"]

  # ---------------------------------------------------------------+
  # Full Gaussian envelope
  # ---------------------------------------------------------------+
  n <- 0:(window_len - 1)
  center <- (window_len - 1) / 2

  if (is.null(sigma_divisor)) {
    sigma <- (window_len + 1) / 3
  } else {
    sigma <- (window_len + 1) / sigma_divisor
  }

  w <- exp(-pi * ((n - center) / sigma)^2)

  # Normalize the complete envelope before boundary truncation
  w_norm <- w / sqrt(sum(w^2))


  # Previous implementation retained for reference.
  # It was replaced because split() internally uses factor(), which introduced
  # substantial overhead for large dictionary blocks.
  #
  ### time_sample <- block[, "time_sample"]
  ### time_groups <- split(seq_len(nrow(block)), time_sample)
  ###
  ### for (idx_in_dict in time_groups) {
  ###   t_sample <- time_sample[idx_in_dict[1L]]

  time_sample <- block[, "time_sample"]

  # Rows belonging to the same time position are normally contiguous.
  # If not, sort them once before processing.
  if (is.unsorted(time_sample)) {
    ord <- order(time_sample)
    time_sorted <- time_sample[ord]
  } else {
    ord <- seq_along(time_sample)
    time_sorted <- time_sample
  }

  # Identify consecutive groups of equal time positions and their lengths
  runs <- rle(time_sorted)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1L

  for (g in seq_along(runs$values)) {

    idx_in_dict <- ord[seq.int(starts[g], ends[g])]
    t_sample <- runs$values[g]

    # ---------------------------------------------------------------+
    # Determine overlap between the complete atom and the signal
    # ---------------------------------------------------------------+
    # Zero-based signal positions occupied by the complete atom
    signal_idx <- t_sample + n

    # Samples lying within the observed signal
    inside <- signal_idx >= 0 & signal_idx < N

    # Corresponding positions within the complete atom/window
    atom_idx <- which(inside)

    # ---------------------------------------------------------------+
    # Zero-padded windowed signal
    # ---------------------------------------------------------------+
    sig_segmented <- matrix(0, nrow = fft_size, ncol = K)

    if (length(atom_idx) > 0L) {
      sig_segmented[atom_idx, ] <- signal[signal_idx[inside] + 1L, , drop = FALSE] *  w_norm[atom_idx]
    }

    # ---------------------------------------------------------------+
    # FFT
    # ---------------------------------------------------------------+
    fft_res <- mvfft(sig_segmented)

    freq_bins <- block[idx_in_dict, "freq_bin"]
    fft_indices <- freq_bins + 1L

    proj_mod_mtx[idx_in_dict, ] <- Mod(fft_res[fft_indices, , drop = FALSE])
    fft_bin_mtx[idx_in_dict, ] <- fft_res[fft_indices, , drop = FALSE]
  }

  return(
    list(
      proj_mod_mtx = proj_mod_mtx,
      fft_bin_mtx = fft_bin_mtx)
  )
}
