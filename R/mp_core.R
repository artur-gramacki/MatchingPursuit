#' Implements the Classic Matching Pursuit (MP) Algorithm
#'
#' Computes a sparse representation of a signal using the classic Matching
#' Pursuit (MP) algorithm and a dictionary of atoms.
#'
#' This is a pure R implementation of the MP algorithm. It is primarily
#' intended for reference and educational purposes and is slower and less
#' numerically precise than the C++ implementation provided with this package.
#' See \code{empi_locate()}, \code{empi_install()}, \code{empi_check()}, and
#' \code{empi_execute()} for information about the C++ implementation.
#'
#' @param dictionary
#' A dictionary of atoms. Can be a matrix, data frame, or any
#' object coercible to a matrix. Atoms are assumed to be stored in columns.
#' Alternatively, a \code{"topk"} object returned by \code{topk_atoms()}.
#'
#' @param signal
#' A signal matrix or an object coercible to a matrix. Signals are
#' assumed to be stored in columns. The signal length (number of rows) must
#' match the atom length.
#'
#' @param channel
#' Index of the signal (channel) to decompose.
#'
#' @param n_nonzero_coefs
#' Maximum number of non-zero coefficients in the sparse representation.
#' If \code{tol = NULL}, the algorithm stops after selecting at most
#' \code{n_nonzero_coefs} atoms. If both \code{n_nonzero_coefs} and
#' \code{tol} are \code{NULL}, the default value is
#' \code{max(1, floor(0.1 * ncol(dictionary)))}. Ignored when
#' \code{tol} is specified.
#'
#' @param tol
#' Stopping tolerance defined as the maximum allowed squared residual
#' norm (\eqn{\|r\|^2}). The algorithm stops when the residual energy falls
#' below this value. If specified, it overrides \code{n_nonzero_coefs}.
#'
#' @param normalize
#' Logical; if \code{TRUE}, dictionary atoms are normalized to
#' unit \eqn{\ell_2} norm before decomposition.
#'
#' @param verbose
#' Logical; flag indicating whether progress information should be printed.
#'
#' @return
#' A list containing the result of the Orthogonal Matching Pursuit
#' decomposition with the following elements:
#'
#' \item{gabors}{Matrix of selected atoms (dictionary columns) used in the
#'   reconstruction.}
#' \item{original_signal}{The original signal reconstructed as a vector.}
#' \item{reconstruction}{The MP approximation of the signal.}
#' \item{coef}{Numeric vector of estimated coefficients for selected atoms.}
#' \item{energy}{Energy contribution of selected atoms, computed as
#'   \code{coef^2 * colSums(selected_atoms^2)}.}
#' \item{support}{Integer vector of selected atom indices.}
#' \item{residual}{Final residual vector.}
#' \item{n_iters}{Number of iterations performed by the algorithm.}
#' \item{error_history}{Current L2 error.}
#'
#' If \code{dictionary} is a \code{"topk"} object, the result additionally
#' contains:
#'
#' \item{frequency}{Frequencies of selected atoms.}
#' \item{phase}{Phases of selected atoms.}
#' \item{scale}{Scales of selected atoms.}
#' \item{position}{Positions of selected atoms.}
#'
#' @export
#'
#' @seealso
#' \code{\link{read_dict}},
#' \code{\link{topk_atoms}},
#' \code{\link{mp_execute}},
#' \code{\link{run_mp_pipeline}}
#'
#' @examples
#' dictionary <- matrix(
#' c(
#'   1.0,  0.9,  0.1,  1.0, -0.2,  0.3,  0.7, -0.5,  1.2,  0.4,
#'   0.2,  1.0,  0.8, -0.3,  1.0, -0.6,  0.5,  0.9, -0.1,  0.8,
#'   0.0,  0.1,  1.0,  0.5,  0.7,  1.1, -0.4,  0.2,  0.6, -0.7,
#'   0.9, -0.2,  0.4,  1.3,  0.1,  0.0,  0.8, -0.9,  0.5,  1.0,
#'  -0.3,  0.6,  1.1, -0.4,  0.2,  0.7, -0.8,  1.0,  0.3,  0.9),
#' nrow = 5, byrow = TRUE
#' )
#'
#' signal <- matrix(
#' c(
#'   4, 3, 5, 2,
#'   2, 1, 2, 3,
#'   3, 2, 4, 1,
#'   5, 4, 3, 2,
#'   1, 3, 2, 4),
#' nrow = 5, byrow = TRUE
#' )
#'
#' # set 'verbose = TRUE' to see the progress
#'
#' fit <- mp_core(
#'   dictionary = dictionary,
#'   signal = signal,
#'   channel = 3,
#'   n_nonzero_coefs = 3,
#'   normalize = TRUE,
#'   verbose = TRUE
#' )
#'
#' fit$coef
#' fit$support
#'
#' # More realistic example, see mp_execute() examples.
#'
#'
mp_core <- function(
    dictionary,
    signal,
    channel = NULL,
    n_nonzero_coefs = NULL,
    tol = NULL,
    normalize = TRUE,
    verbose = FALSE
) {

  if (inherits(dictionary, "topk")) {
    D <- dictionary$atoms[[channel]]
    D <- as.matrix(D)
  }

  if (!inherits(dictionary, "topk")) {
    if (is.vector(dictionary) || is.data.frame(dictionary) || is.matrix(dictionary)) {
      D <- as.matrix(dictionary)
    } else {
      stop("'dictionary' must be a matrix or convertible to a matrix.")
    }
  }

  if (is.vector(signal) || is.data.frame(signal) || is.matrix(signal)) {
    signal <- as.matrix(signal)
  } else {
    stop("'signal' must be a matrix or convertible to a matrix.")
  }

  n <- nrow(D)
  p <- ncol(D)

  sig <- signal[, channel]
  sig <- as.numeric(sig)

  if (length(sig) != n) {
    stop("Dimension mismatch between dictionary and signal.")
  }

  if (!is.null(n_nonzero_coefs) && p < n_nonzero_coefs) {
    stop("The number of atoms cannot be more than the number of features.")
  }

  if (is.null(tol)) {
    if (is.null(n_nonzero_coefs)) {
      # '1' is a minimal constraint ensuring at least one atom is selected and
      # the representation is non-empty.
      # Example: For a small dictionary, e.g. ncol(D) = 8, 0.1 × 8 = 0.8.
      n_nonzero_coefs <- max(1, floor(0.1 * p))
    }
    max_iter <- min(n_nonzero_coefs, p)
  } else {
    # tol overrides n_nonzero_coefs
    max_iter <- p
  }

  # 1. Normalize the dictionary columns (atoms must have an L2 norm of 1)
  # Often, the dictionary is already normalized, but this makes the calculations safer
  if (normalize) {
    D_norm <- apply(D, 2, function(col) col / sqrt(sum(col^2)))
  } else {
    D_norm = D
  }

  residual <- sig
  n_atoms <- ncol(D)

  # Integer vector of selected atom indices.
  support <- integer(0)

  # Initialize the coefficient vector (sparse vector representation)
  coef <- rep(0, n_atoms)

  # History of reconstruction error
  error_history <- c(sum(residual^2))

  for (k in 1:max_iter) {
    # 2. Calculating the scalar products of the remainder with all atoms
    projections <- t(D_norm) %*% residual

    #3. Selecting the atom with the best fit (largest absolute value)
    best_atom_idx <- which.max(abs(projections))
    best_projection <- projections[best_atom_idx]

    if (verbose) message("iteration: ", k, ", selected atom: ", best_atom_idx)

    # 4. Update the coefficient for the selected atom
    # (in classic MP, coefficients stack if the same atom is selected again)
    coef[best_atom_idx] <- coef[best_atom_idx] + best_projection

    # 5. Calculating the new remainder
    residual <- residual - best_projection * D_norm[, best_atom_idx]

    # Save current L2 error
    current_error <- sum(residual^2)
    error_history <- c(error_history, current_error)

    support <- c(support, best_atom_idx)

    # Stop criterion: if error is less than tol
    if (!is.null(tol)) {
      if (current_error < tol) {
        message("Convergence achieved in iteration:", k, "\n")
        break
      }
    }
  }

  selected_atoms <- D_norm[, support]
  coefs_selected  <- coef[support]
  energy <- coefs_selected^2 * colSums(selected_atoms^2)

  if (inherits(dictionary, "topk")) {
    frequency <- dictionary$frequency[support, channel]
    phase <- dictionary$phase[support, channel]
    scale <- dictionary$scale[support, channel]
    position <- dictionary$position[support, channel]
  }

  if (inherits(dictionary, "topk")) {
    list(
      gabors = selected_atoms,
      original_signal = sig,
      reconstruction = as.vector(sig - residual),
      coefs = coefs_selected,
      energy = energy,
      support = support,
      residual = as.vector(residual),
      error_history = error_history,
      n_iters = k,
      frequency = frequency,
      phase = phase,
      scale = scale,
      position = position
    )
  } else {
    list(
      gabors = selected_atoms,
      original_signal = sig,
      reconstruction = as.vector(sig - residual),
      coefs = coefs_selected,
      energy = energy,
      support = support,
      residual = as.vector(residual),
      error_history = error_history,
      n_iters = k
    )
  }
}
