#' Implements Orthogonal Matching Pursuit (OMP) algorithm
#'
#' Performs Orthogonal Matching Pursuit (OMP) to obtain a sparse
#' representation of a signal using a dictionary of candidate atoms.
#'
#' Unlike classical Matching Pursuit, OMP recomputes the coefficients of all
#' previously selected atoms at each iteration by solving a least-squares
#' problem. This makes the residual orthogonal to the subspace spanned by the
#' selected atoms and generally provides a more accurate approximation for a
#' given number of atoms.
#'
#' The least-squares problem is solved efficiently using incremental Cholesky
#' factorization.
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
#' \code{tol} is specified.
#'
#' @param tol Optional stopping tolerance for the relative residual energy,
#'   defined as
#'   \deqn{\|r_k\|_2^2 / \|x\|_2^2.}
#'   The algorithm stops when this value is less than or equal to
#'   \code{tol}. If specified, \code{tol} overrides
#'   \code{n_nonzero_coefs}.
#'
#' @param verbose
#' Logical; flag indicating whether progress information should be printed.
#'
#' @details
#' At each iteration, the atom with the largest absolute correlation with the
#' current residual is selected. All coefficients associated with the selected
#' atoms are then recomputed simultaneously by least squares, and the residual
#' is updated.
#'
#' For OMP, the quantity stored in \code{energy} is not calculated from the
#' final coefficient of an individual atom. Selected atoms can be strongly
#' correlated, and therefore quantities such as
#' \code{coefs^2 * colSums(selected_atoms^2)} are not additive contributions
#' to the energy of the reconstruction.
#'
#' Instead, the energy associated with the atom selected at iteration
#' \eqn{k} is defined as the reduction in residual energy:
#'
#' \deqn{
#' E_k = \|r_{k-1}\|_2^2 - \|r_k\|_2^2.
#' }
#'
#' Thus, \code{energy[k]} quantifies the reduction in residual energy
#' associated with the \eqn{k}-th OMP iteration, after adding a new atom
#' and re-estimating all active coefficients.
#'
#' \deqn{
#' \sum_k E_k =
#' \|x\|_2^2 - \|r_K\|_2^2.
#' }
#'
#' @return A list containing the result of the Orthogonal Matching Pursuit
#' decomposition with the following elements:
#'
#' \item{selected_atoms}{
#'   Matrix of selected unit-L2-normalized dictionary atoms used in the reconstruction.}
#'
#' \item{signal}{
#'   The analyzed signal channel returned as a numeric vector.}
#'
#' \item{reconstruction}{
#'   OMP reconstruction of the signal}
#'
#' \item{coefs}{
#'   Final least-squares coefficients corresponding to
#'   \code{selected_atoms}.}
#'
#' \item{energy}{
#'   Numeric vector containing the reduction in residual energy produced at
#'   each OMP iteration,
#'   \eqn{\|r_{k-1}\|_2^2 - \|r_k\|_2^2}.
#' }
#' \item{support}{
#'   Indices of the selected atoms in the input dictionary, in selection
#'   order.}
#'
#' \item{residual}{
#'   Final residual vector.}
#'
#' \item{relative_residual_energy}{
#'   Relative residual energy after initialization and after each OMP
#'   iteration.}
#'
#' \item{n_iters}{
#'   Number of OMP iterations performed.}
#'
#' @seealso
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{mp_core}},
#' \code{\link{mp_omp_execute}},
#' \code{\link{tf_map}}
#'
#' @export
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
#' fit <- omp_core(
#'   dictionary = dictionary,
#'   signal = signal,
#'   channel = 1,
#'   n_nonzero_coefs = 3,
#'   verbose = TRUE
#' )
#'
#' fit$coefs
#' # [1] 5.282278 2.637693 2.195920
#'
#' fit$support
#' # [1] 9 5 7
#'
#' fit$relative_residual_energy
#' # [1] 1.00000000 0.28422833 0.16609350 0.08795047
#'
#' # For a complete Gabor decomposition workflow, see mp_omp_execute().
#'

omp_core <- function(
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

  if (channel < 1L || channel > ncol(signal)) {
    stop("'channel' is out of range.")
  }

  # Dictionary
  if (is.vector(dictionary) || is.data.frame(dictionary) || is.matrix(dictionary)) {
    D <- as.matrix(dictionary)
  } else {
      stop("'dictionary' must be a matrix or convertible to a matrix.")
  }

  n <- nrow(D)
  p <- ncol(D)

  sig <- as.numeric(signal[, channel])
  sig_original <- sig

  if (length(sig) != n) {
    stop("Dimension mismatch between dictionary and signal.")
  }

  if (any(!is.finite(sig))) {
    stop("Signal contains non-finite values.")
  }

  total_energy <- sum(sig^2)

  if (total_energy < 1e-12) {
    stop("Signal has zero energy.")
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
    max_iter <- min(n_nonzero_coefs, p)
  } else {
    # tol overrides n_nonzero_coefs
    # When tol is supplied, stopping is controlled by residual energy,
    # therefore all dictionary atoms are potentially available.
    max_iter <- p
  }

  norms <- sqrt(colSums(D^2))
  if (any(!is.finite(norms)) || any(norms < 1e-12)) {
    stop("Dictionary contains non-finite or zero/near-zero norm atoms.")
  }

  # Pre-compute Dtsig only
  # D' * signal does not change during OMP, so compute it only once.
  # It is later used in the least-squares solution for the active set.
  Dtsig <- as.vector(crossprod(D, sig)) / norms

  # Outputs
  support <- integer(0)
  coefs <- rep(0, p)
  residual <- sig
  L <- NULL

  # IMPORTANT:
  # Store the absolute residual energy at every OMP iteration.
  # Apart from diagnostics, this is needed to define the energy assigned
  # to each selected atom as the DECREASE in residual energy:
  #
  #   E_k = ||r_{k-1}||^2 - ||r_k||^2
  #
  # Do not replace this later with beta_k^2 * ||d_k||^2. In OMP the
  # coefficients of ALL selected atoms are re-estimated at every iteration.
  # For correlated atoms, individual beta_k^2 values can therefore become
  # very large and are not additive contributions to reconstruction energy.
  residual_energy <- numeric(max_iter + 1)
  residual_energy[1] <- sum(residual^2)

  # Relative residual energy is useful as a scale-independent convergence
  # measure. Index 1 corresponds to the residual BEFORE the first atom
  # has been selected.
  relative_residual_energy <- numeric(max_iter + 1)
  relative_residual_energy[1] <- residual_energy[1] / total_energy

  # Main OMP loop
  n_iters <- 0L
  for (k in seq_len(max_iter)) {

    # 1. Select the atom most correlated with the current residual
    corr <- as.vector(crossprod(D, residual)) / norms

    # Previously selected atoms must not be selected again.
    if (length(support) > 0) {
      corr[support] <- 0
    }

    j <- which.max(abs(corr))

    # 2. Incrementally update the Cholesky factor
    # Instead of recomputing the complete Gram matrix and solving the
    # least-squares problem from scratch, update the Cholesky factor
    # when one new atom enters the active set.
    if (k == 1) {
      L_new <- matrix(1, nrow = 1L, ncol = 1L)
    } else {
      # lazy Gram computation
      # Correlations of the new atom with previously selected atoms.
      w <- as.vector(crossprod(D[, support, drop = FALSE], D[, j])) / (norms[support] * norms[j])

      # Solve L v = w
      v <- forwardsolve(L, w)
      alpha <- 1 - sum(v^2)

      # Schur-complement term. A value close to zero means that the new
      # atom is almost linearly dependent on the active dictionary.
      if (alpha <= 1e-12) {
        warning(paste("Near linear dependence detected at iteration", k))
        break
      }

      L_new <- rbind(cbind(L, rep(0, nrow(L))), c(v, sqrt(alpha)))
    }

    support <- c(support, j)
    L <- L_new

    # 3. Re-estimate ALL active coefficients by least squares
    # This is the essential difference between OMP and classical MP.
    # Previously estimated coefficients may change when a new atom
    # is added to the active set.
    b <- Dtsig[support]
    z <- forwardsolve(L, b)
    x_active <- as.numeric(backsolve(t(L), z))

    # 4. Build the full sparse coefficient vector
    if (verbose) message("iteration: ", k, ", selected atom: ", j)

    # 5. Update residual
    residual <- sig -  D[, support, drop = FALSE] %*% (x_active / norms[support])

    # 6. Store residual energy and check stopping criterion
    residual_sq_norm <- sum(residual^2)

    # Keep this history: consecutive differences are later used as the
    # energy contributions assigned to selected OMP atoms.
    residual_energy[k + 1L] <- residual_sq_norm
    relative_residual_energy[k + 1L] <- residual_sq_norm / total_energy

    n_iters <- k

    if (!is.null(tol)) {
      if (verbose) message("relative residual energy: ", signif(relative_residual_energy[k + 1L], 6))

      if (relative_residual_energy[k + 1L] <= tol) {
        if (verbose) message("Convergence achieved in iteration: ", k)
        break
      }
    }
  }

  selected_atoms <- sweep(D[, support, drop = FALSE], 2, norms[support], "/")
  coefs_selected  <- x_active

  # Energy assigned to selected OMP atoms
  # IMPORTANT:
  # In OMP, do NOT use
  #
  #   energy <- coefs_selected^2 * colSums(selected_atoms^2)
  #
  # as an additive "energy contribution". Because active coefficients
  # are jointly re-estimated, strongly correlated atoms can obtain very
  # large individual coefficients that partially cancel each other.
  #
  # Instead, assign to the atom selected at iteration k the actual
  # reduction in residual energy obtained in that iteration:
  #
  #   E_k = ||r_{k-1}||^2 - ||r_k||^2
  #
  # Consequently,
  #
  #   sum(E_k) = ||x||^2 - ||r_K||^2.
  #
  # This definition also gives meaningful intensities for tf_map().
  energy <- residual_energy[seq_len(n_iters)] - residual_energy[2:(n_iters + 1L)]

  relative_residual_energy <- relative_residual_energy[seq_len(n_iters + 1L)]

  # Output
  list(
    selected_atoms = selected_atoms,
    signal = sig_original,
    reconstruction = as.vector(sig_original - residual),
    coefs = coefs_selected,
    energy = energy,
    support = support,
    residual = as.vector(residual),
    relative_residual_energy = relative_residual_energy,
    n_iters = n_iters
  )
}





