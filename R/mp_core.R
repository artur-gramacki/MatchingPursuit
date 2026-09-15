#' Implements the classical Matching Pursuit (MP) algorithm
#'
#' Computes a sparse representation of a signal using the classical Matching
#' Pursuit (MP) algorithm and a dictionary of atoms.
#'
#' This is a native R implementation of the classical MP algorithm supporting
#' arbitrary matrix-based dictionaries. It provides direct access to the
#' decomposition procedure and can be used independently of the Gabor-specific
#' workflow. For high-performance Matching Pursuit decomposition using the
#' external EMPI backend, see \code{empi_locate()}, \code{empi_install()},
#' \code{empi_check()}, and \code{empi_execute()}.
#'
#' @param dictionary
#' A dictionary of atoms. Can be a numeric vector, matrix, or data frame.
#' Atoms are assumed to be stored in columns.
#' Dictionary atoms are internally normalized to unit L2 norm before decomposition.
#' Therefore, atom selection is invariant to non-zero scaling of dictionary columns.
#'
#' @param signal
#' Can be a numeric vector, matrix, or data frame. Signals are
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
#' \code{tol} is specified. Note that in classical
#' MP, the same dictionary atom may be selected more than once.
#'
#' @param tol
#' Stopping tolerance expressed as the maximum allowed relative residual
#' energy, \eqn{\|r\|_2^2 / \|x\|_2^2}. The algorithm stops when the residual
#' energy falls below this value. If specified, it overrides \code{n_nonzero_coefs}.
#'
#' @param verbose
#' Logical; flag indicating whether progress information should be printed.
#'
#' @return
#' A list containing the result of the Matching Pursuit
#' decomposition with the following elements:
#'
#' \item{selected_atoms}{Matrix of selected unit-L2-normalized dictionary
#' atoms used in the reconstruction.}
#'
#' \item{signal}{The analyzed signal channel returned as a numeric vector.}
#'
#' \item{reconstruction}{The MP approximation of the signal.}
#'
#' \item{coefs}{Numeric vector of estimated coefficients for selected atoms.}
#'
#' \item{energy}{
#' Numeric vector containing the reduction in squared residual norm
#' associated with each pursuit iteration,
#' \eqn{E_k = \|r_{k-1}\|_2^2 - \|r_k\|_2^2}.}
#'
#' \item{support}{Integer vector of selected atom indices at every iteration.}
#'
#' \item{residual}{Final residual vector.}
#'
#' \item{relative_residual_energy}{
#' Relative residual energy, \eqn{\|r_k\|_2^2 / \|x\|_2^2}, including the
#' initial value before the first iteration and the value after each Matching
#' Pursuit iteration. Values close to zero indicate a better reconstruction.}
#'
#' \item{n_iters}{Number of iterations performed by the algorithm.}
#'
#' @details
#' Dictionary atoms are normalized internally to unit L2 norm. For classical
#' MP, the reduction in squared residual norm at iteration \eqn{k} is therefore
#' theoretically equal to the squared MP coefficient. The former definition
#' is used explicitly to maintain a consistent interpretation of
#' \code{energy} across MP and OMP decompositions.
#'
#' @export
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{mp_omp_execute}}
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
#' fit <- mp_core(
#'   dictionary = dictionary,
#'   signal = signal,
#'   channel = 1,
#'   n_nonzero_coefs = 3,
#'   verbose = TRUE
#' )
#'
#' fit$coefs
#' # [1] 6.274348 2.535423 1.870568
#'
#' fit$support
#' # [1] 9 5 7
#'
#' fit$relative_residual_energy
#' # [1] 1.0000000 0.2842283 0.1673489 0.1037303
#'
#' # More realistic example, see mp_omp_execute() examples.
#'
mp_core <- function(
    dictionary,
    signal,
    channel = NULL,
    n_nonzero_coefs = NULL,
    tol = NULL,
    verbose = FALSE
) {

  # Signal
  if (is.vector(signal) || is.data.frame(signal) || is.matrix(signal)) {
    signal <- as.matrix(signal)
  } else {
    stop("'signal' must be a matrix or convertible to a matrix.")
  }

  # Channel
  if (is.null(channel)) {
    if (ncol(signal) == 1L) {
      channel <- 1L
    } else {
      stop("'channel' must be specified for multichannel signals.")
    }
  }

  if (channel < 1 || channel > ncol(signal)) {
    stop("'channel' is out of range.")
  }

  if (is.vector(dictionary) || is.data.frame(dictionary) || is.matrix(dictionary)) {
    D <- as.matrix(dictionary)
  } else {
    stop("'dictionary' must be a matrix or convertible to a matrix.")
  }

  # Normalize dictionary atoms to unit L2 norm.
  # This makes atom selection independent of arbitrary column scaling
  # and provides a common representation for arbitrary dictionaries.
  norms <- sqrt(colSums(D^2))

  if (any(!is.finite(norms)) || any(norms < 1e-12)) {
    stop("Dictionary contains non-finite or zero/near-zero norm atoms.")
  }

  n <- nrow(D)
  p <- ncol(D)

  sig <- as.numeric(signal[, channel])

  if (length(sig) != n) {
    stop("Dimension mismatch between dictionary and signal.")
  }

  if (!is.null(n_nonzero_coefs)) {
    if (length(n_nonzero_coefs) != 1L ||
        !is.finite(n_nonzero_coefs) ||
        n_nonzero_coefs < 1 ||
        n_nonzero_coefs != as.integer(n_nonzero_coefs)) {
      stop("'n_nonzero_coefs' must be a positive integer.")
    }

    if (n_nonzero_coefs > p) {
      stop("'n_nonzero_coefs' cannot exceed the number of dictionary atoms.")
    }
  }

  if (!is.null(tol)) {
    if (length(tol) != 1L ||
        !is.finite(tol) ||
        tol < 0 ||
        tol > 1) {
      stop("'tol' must be a finite number between 0 and 1.")
    }
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

  # D_norm <- apply(D, 2, function(col) col / sqrt(sum(col^2)))
  # simpler:
  # Don't need D_norm, as: <D / ||D||, residual>  =  <D, residual> / ||D|| (***)
  # D_norm <- sweep(D, 2, norms, "/")

  # Integer vector of selected atom indices.
  support <- integer(max_iter)

  # MP coefficient history
  coefs <- numeric(max_iter)

  total_energy <- sum(sig^2)
  if (total_energy < 1e-12) stop("Signal has zero energy.")

  residual <- sig
  residual_energy <- numeric(max_iter + 1L)
  residual_energy[1L] <- sum(residual^2)

  # History of relative residual energy.
  relative_residual_energy <- numeric(max_iter + 1L)
  relative_residual_energy[1] <- residual_energy[1L] / total_energy

  for (k in 1:max_iter) {
    # Compute correlations between all unit-norm dictionary atoms
    # and the current residual.
    projections <- as.vector(crossprod(D, residual)) / norms


    # Selecting the atom with the best fit (largest absolute value)
    # Classical MP may select the same atom more than once.
    best_atom_idx <- which.max(abs(projections))
    best_projection <- projections[best_atom_idx]

    coefs[k] <- best_projection
    support[k] <- best_atom_idx

    if (verbose) {
      message("iteration: ", k, ", selected atom: ", best_atom_idx, ", coefficient: ", signif(best_projection, 6)
      )
    }

    # Matching Pursuit residual update.
    residual <- residual - best_projection * D[, best_atom_idx] / norms[best_atom_idx]

    # Store absolute and relative residual energy.
    residual_energy[k + 1L] <- sum(residual^2)
    relative_residual_energy[k + 1L] <-  residual_energy[k + 1L] / total_energy

    # Stop when the relative residual energy falls below the tolerance.
    if (!is.null(tol)) {
      if (verbose) {
        message("relative residual energy: ", signif(relative_residual_energy[k + 1L], 6))
      }
      if (relative_residual_energy[k + 1L] <= tol) {
        if (verbose) message("Convergence achieved in iteration: ", k)
        break
      }
    }
  }

  # If terminate the loop early with 'tol', then 'support' has length max_iter,
  # but only the first k elements are filled (are != zero)
  support <- support[seq_len(k)]
  coefs <- coefs[seq_len(k)]

  relative_residual_energy <- relative_residual_energy[seq_len(k + 1L)]
  # for k = 1 it can return a vector, not a matrix. Then 'drop = FALSE' prevents this

  # we normalize only the atoms actually selected by MP, not the entire dictionary.
  selected_atoms <- sweep(D[, support, drop = FALSE], 2, norms[support], "/")

  # Energy attributed to iteration k is defined as the decrease in
  # squared residual norm:
  #
  #   E_k = ||r_{k-1}||^2 - ||r_k||^2.
  #
  # For classical MP with unit-L2-norm atoms this is theoretically
  # equivalent to coefs[k]^2. We compute it explicitly from the residual
  # energies to use the same definition of iteration energy in MP and OMP.
  # In OMP, coefficients of previously selected atoms are re-estimated at
  # every iteration, so squared coefficients cannot generally be interpreted
  # as the energy reduction associated with a particular iteration.
  energy <- residual_energy[seq_len(k)] -  residual_energy[2:(k + 1L)]

  list(
    selected_atoms = selected_atoms,
    signal = sig,
    reconstruction = as.vector(sig - residual),
    coefs = coefs,
    energy = energy,
    support = support,
    residual = as.vector(residual),
    relative_residual_energy = relative_residual_energy,
    n_iters = k
  )
}
