#' Implements the Classical Matching Pursuit (MP) Algorithm
#'
#' Computes a sparse representation of a signal using the classical Matching
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
#' \item{coefs}{Numeric vector of estimated coefficients for selected atoms.}
#' \item{energy}{Energy contribution of selected atoms, computed as \code{coefs^2}.}
#' \item{support}{Integer vector of selected atom indices at every iteration.}
#' \item{residual}{Final residual vector.}
#' \item{n_iters}{Number of iterations performed by the algorithm.}
#' \item{relative_residual_energy}{Fraction of the original signal energy that
#' remains unexplained after each Matching Pursuit iteration. Values close to zero
#' indicate a better reconstruction.}
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
#' \code{\link{mp_omp_execute}},
#' \code{\link{mp_omp_run_pipeline}}
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
#'   channel = 1,
#'   n_nonzero_coefs = 3,
#'   normalize = TRUE,
#'   verbose = TRUE
#' )
#'
#' # More realistic example, see mp_omp_execute() examples.
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

  is_topk <- inherits(dictionary, "topk")

  if (is.null(channel))  stop("'channel' must be specified.")

  if (is_topk) {
    D <- dictionary$atoms[[channel]]
    D <- as.matrix(D)
  }

  if (!is_topk) {
    if (is.vector(dictionary) || is.data.frame(dictionary) || is.matrix(dictionary)) {
      D <- as.matrix(dictionary)
    } else {
      stop("'dictionary' must be a matrix or convertible to a matrix.")
    }
  }

  norms <- sqrt(colSums(D^2))

  if (any(norms < 1e-12)) stop("Dictionary contains zero or near-zero norm atoms.")

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
    max_iter <- n_nonzero_coefs
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

  # Integer vector of selected atom indices.
  support <- integer(max_iter)

  # MP coefficient history
  coefs <- numeric(max_iter)

  total_energy <- sum(sig^2)
  if (total_energy < 1e-12) stop("Signal has zero energy.")

  # History of reconstruction error
  relative_residual_energy <- numeric(max_iter + 1)
  relative_residual_energy[1] <- sum(residual^2) / total_energy

  for (k in 1:max_iter) {
    # 2. Calculating the scalar products of the remainder with all atoms
    projections <- t(D_norm) %*% residual

    #3. Selecting the atom with the best fit (largest absolute value)
    best_atom_idx <- which.max(abs(projections))
    best_projection <- projections[best_atom_idx]

    if (verbose) message("iteration: ", k, ", selected atom: ", best_atom_idx)

    # 4. Update the coefficient for the selected atom
    # (in classical MP, coefficients stack if the same atom is selected again)
    ###coefs[best_atom_idx] <- coefs[best_atom_idx] + best_projection
    coefs[k] <- best_projection
    support[k] <- best_atom_idx

    # 5. Calculating the new remainder
    residual <- residual - best_projection * D_norm[, best_atom_idx]

    # Save current L2 error
    residual_sq_norm <- sum(residual^2)
    relative_residual_energy[k + 1] <- residual_sq_norm / total_energy

    # Stop criterion: squared L2 norm of residual < tol
    if (!is.null(tol)) {
      if (verbose) message("relative residual energy: ", signif(relative_residual_energy[k + 1], 6))
      if (relative_residual_energy[k + 1] < tol) {
        message("Convergence achieved in iteration: ", k, "\n")
        break
      }
    }
  }

  # If terminate the loop early with 'tol', then 'support' has length max_iter,
  # but only the first k elements are filled (are != zero)
  support <- support[1:k]
  coefs <- coefs[1:k]
  relative_residual_energy <- relative_residual_energy[1:(k + 1)]
  # for k = 1 it can return a vector, not a matrix. Then 'drop = FALSE' prevents this
  gabors = D_norm[, support, drop = FALSE]

  # as atoms are normalized ||g||= 1, colSums(gabors^2) is always 1
  # energy <- coefs^2 * colSums(gabors^2)
  energy <- coefs^2

  if (is_topk) {
    frequency <- dictionary$frequency[support, channel]
    phase <- dictionary$phase[support, channel]
    scale <- dictionary$scale[support, channel]
    position <- dictionary$position[support, channel]
  }

  if (is_topk) {
    list(
      gabors = gabors,
      original_signal = sig,
      reconstruction = as.vector(sig - residual),
      #reconstruction2 = gabors %*% coefs,
      coefs = coefs,
      energy = energy,
      support = support,
      residual = as.vector(residual),
      relative_residual_energy = relative_residual_energy,
      n_iters = k,
      frequency = frequency,
      phase = phase,
      scale = scale,
      position = position
    )
  } else {
    list(
      gabors = gabors,
      original_signal = sig,
      reconstruction = as.vector(sig - residual),
      #reconstruction2 = gabors %*% coefs,
      coefs = coefs,
      energy = energy,
      support = support,
      residual = as.vector(residual),
      relative_residual_energy = relative_residual_energy,
      n_iters = k
    )
  }
}
