#' Materialize selected Gabor atoms efficiently
#'
#' @description
#' Reconstructs time-domain Gabor atoms selected by
#' \code{topk_gabor_atoms()} from the parameters stored in a \code{"topk"}
#' object.
#'
#' This optimized implementation caches Gaussian envelopes for repeated
#' support lengths and reconstructs each oscillatory carrier directly as
#' \code{cos(theta + phi)}. This preserves the same numerical evaluation used
#' in the original materialized implementation.
#'
#' @param x An object of class \code{"topk"} returned by \code{topk_gabor_atoms()}.
#'
#' @param channel Positive integer specifying the signal channel for which the
#'   atom dictionary should be reconstructed. Defaults to \code{1}.
#'
#' @param sparse Logical; if \code{TRUE}, the reconstructed atom dictionary is
#'   returned as a sparse matrix. If \code{FALSE}, a standard dense matrix is
#'   returned. Defaults to \code{TRUE}.
#'
#' @param verbose Logical; if \code{TRUE}, the size of the reconstructed atom
#'   matrix is printed in MB. Defaults to \code{FALSE}.
#'
#' @details
#' Each selected atom is reconstructed from its stored frequency, optimal
#' phase, zero-based start position, support length, sampling frequency, and
#' Gaussian-envelope divisor. The complete atom is normalized before any part
#' extending beyond the observed signal boundaries is removed.
#'
#' Gaussian envelopes are cached by support length because atoms with the same
#' support length share the same local sample vector and Gaussian envelope.
#' The oscillatory carrier is evaluated directly for every atom as
#' \code{cos(theta + phi)}, where \code{theta} depends on frequency and local
#' sample position and \code{phi} is the stored optimal phase.
#'
#' When \code{sparse = TRUE}, the number of signal samples occupied by each
#' selected atom is determined before reconstruction. These overlap lengths are
#' used to preallocate the column pointers, row indices, and numerical values
#' required for the sparse representation. The sparse matrix is assembled once
#' at the end using \code{Matrix::sparseMatrix()}, without constructing an
#' intermediate dense dictionary. Exact zero values that may occur within an
#' atom support are subsequently removed with \code{Matrix::drop0()}.
#'
#' If all selected atoms lie completely within the observed signal boundaries,
#' boundary checks are skipped during reconstruction. Otherwise, only the
#' contiguous fragment of each atom overlapping the signal is stored.
#'
#' When \code{sparse = FALSE}, a dense matrix of dimensions
#' \code{x$signal_length} by the number of selected atoms is allocated once,
#' and reconstructed atom fragments are written directly into its columns.
#'
#' @return A sparse or dense matrix with \code{x$signal_length} rows and one
#'   column for each selected atom. Columns correspond to the selected atoms
#'   in the same order as in the parameter matrices stored in \code{x}.
#'
#' @export
#'
#' @seealso
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{gabor_atoms_matrix}},
#' \code{\link{mp_core}},
#' \code{\link{omp_core}}
#'
#' @examples
#' \dontrun{
#' # topk_atoms is an object returned by topk_gabor_atoms()
#' D_sparse <- gabor_atoms_matrix(topk_atoms)
#' D_dense <- gabor_atoms_matrix(topk_atoms, sparse = FALSE)
#' }
#'
gabor_atoms_matrix <- function(x, channel = 1L, sparse = TRUE, verbose = FALSE) {

  if (!inherits(x, "topk")) {
    stop("'x' must be an object of class 'topk'.")
  }

  if (length(channel) != 1L ||
      !is.finite(channel) ||
      channel < 1L ||
      channel != as.integer(channel)) {
    stop("'channel' must be a positive integer.")
  }

  if (!is.logical(sparse) || length(sparse) != 1L || is.na(sparse)) {
    stop("'sparse' must be TRUE or FALSE.")
  }

  # Parameters required to reconstruct the atoms.
  required <- c(
    "frequency",
    "phase",
    "atom_begin_sample",
    "window_len_samples",
    "sampling_frequency",
    "signal_length",
    "sigma_divisor"
  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0L) {
    stop(
      "The 'topk' object does not contain the parameters required to materialize Gabor atoms: ",
      paste(missing, collapse = ", "),
      "."
    )
  }

  n_channels <- ncol(x$frequency)

  if (channel > n_channels) stop("'channel' cannot be greater than ", n_channels, ".")

  # Extract parameters for the selected channel ----
  N <- as.integer(x$signal_length)
  sampling_frequency <- x$sampling_frequency
  sigma_divisor <- x$sigma_divisor

  freq_vec <- x$frequency[, channel]
  phase_vec <- x$phase[, channel]
  atom_begin_sample_vec <- as.integer(x$atom_begin_sample[, channel])
  window_len_samples_vec <- as.integer(x$window_len_samples[, channel])

  topk <- length(freq_vec)

  # Cache quantities that depend only on window length ----
  # Atoms with the same support length have the same local sample vector n
  # and the same Gaussian envelope. Calculate these quantities only once for
  # each distinct window length.
  window_lengths <- unique(window_len_samples_vec)
  window_id <- match(window_len_samples_vec, window_lengths)
  n_cache <- vector("list", length(window_lengths))
  w_cache <- vector("list", length(window_lengths))

  for (k in seq_along(window_lengths)) {
    window_len <- window_lengths[k]
    n <- 0:(window_len - 1L)
    center <- (window_len - 1) / 2
    sigma <- (window_len + 1) / sigma_divisor

    n_cache[[k]] <- n
    w_cache[[k]] <- exp(-pi * ((n - center) / sigma)^2)
  }

  # Fast path: TRUE when every selected atom lies completely inside the
  # observed signal, so no boundary truncation is required.
  all_inside <- all(
    atom_begin_sample_vec >= 0L &
    atom_begin_sample_vec + window_len_samples_vec <= N
  )

  # Allocate output storage ----
  if (sparse) {
    # Determine how many support samples of each atom are stored in the signal.
    # If all atoms are fully inside, this is simply the complete window length.
    # Otherwise, calculate the exact overlap of each support with [0, N).
    # nnz - number of non-zero elements
    if (all_inside) {
      nnz_per_atom <- window_len_samples_vec
    } else {
      overlap_begin <- pmax.int(atom_begin_sample_vec, 0L)
      overlap_end <- pmin.int(atom_begin_sample_vec + window_len_samples_vec, N)
      nnz_per_atom <- pmax.int(overlap_end - overlap_begin, 0L)
    }

    # Preallocate the compressed-column representation. Here nnz denotes the
    # number of support entries stored before exact numerical zeros are removed
    # by Matrix::drop0().
    p <- c(0L, cumsum(nnz_per_atom))
    nnz <- p[topk + 1L]

    row_idx <- integer(nnz)
    atom_values <- numeric(nnz)
  } else {
    # Dense output is allocated once. Atom fragments are written directly into
    # their final columns during reconstruction.
    atoms_mtx <- matrix(0, nrow = N, ncol = topk)
  }

  # Reconstruct selected atoms ----

  for (j in seq_len(topk)) {
    t_sample <- atom_begin_sample_vec[j]
    window_len <- window_len_samples_vec[j]
    k_window <- window_id[j]

    # Reuse the local sample vector and Gaussian envelope for this support
    # length instead of recalculating them for every atom.
    n <- n_cache[[k_window]]
    w <- w_cache[[k_window]]

    # Evaluate the oscillatory carrier directly, using the same expression as
    # in the original materialized implementation.
    carrier <- cos(2 * pi * freq_vec[j] * n / sampling_frequency + phase_vec[j])

    # Apply the Gaussian envelope.
    atom <- w * carrier

    # Normalize the complete atom before boundary truncation. This preserves
    # the same normalization convention as the original materialized version.
    atom_norm <- sqrt(sum(atom^2))

    if (atom_norm > 0) atom <- atom / atom_norm

    if (all_inside) {
      # Complete support is inside the signal. Convert the zero-based support
      # positions to one-based R row indices.
      atom_rows <- t_sample + n + 1L
      atom_part <- atom
    } else {
      # Determine the contiguous part of the complete atom that overlaps the
      # signal without constructing a full logical boundary mask.
      n_first <- max(0L, -t_sample)
      n_last <- min(window_len - 1L, N - 1L - t_sample)

      if (n_first > n_last) next

      atom_idx <- seq.int(n_first + 1L, n_last + 1L)
      atom_rows <- t_sample + atom_idx
      atom_part <- atom[atom_idx]
    }

    if (sparse) {
      # Write the current atom directly into its preallocated segment of the
      # compressed-column data. No sparse matrix is modified incrementally.
      from <- p[j] + 1L
      to <- p[j + 1L]

      row_idx[from:to] <- atom_rows
      atom_values[from:to] <- atom_part
    } else {
      # Dense path: write the atom fragment directly into its final column.
      atoms_mtx[atom_rows, j] <- atom_part
    }
  }

  # Assemble sparse matrix ----
  if (sparse) {
    # Construct the sparse matrix once from the preallocated compressed-column
    # components. No intermediate dense dictionary is created.
    atoms_mtx <- Matrix::sparseMatrix(
      i = row_idx,
      p = p,
      x = atom_values,
      dims = c(N, topk)
    )
    # Remove exact numerical zeros that may occur inside an atom support,
    # e.g. at zero crossings of the oscillatory carrier.
    atoms_mtx <- Matrix::drop0(atoms_mtx)
  }

  if (verbose) {
    size_mb <- as.numeric(utils::object.size(atoms_mtx)) / 1024^2
    matrix_type <- if (sparse) "sparse" else "dense"
    message(
      "gabor_atoms_matrix(): ", matrix_type,
      " matrix created, size = ", sprintf("%.2f", size_mb), " MB."
    )
  }

  return(atoms_mtx)
}
