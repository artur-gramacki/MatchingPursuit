#' Implements Orthogonal Matching Pursuit (OMP) algorithm
#'
#' This function implements the Orthogonal Matching Pursuit (OMP) algorithm to
#' compute a sparse representation of a signal using a dictionary of atoms.
#' This is an efficient implementation with incremental Cholesky factorization for
#' efficient least-squares solving. The function works very similarly to the
#' Python implementation of OMP available in the scikit-learn library.
#'
#' Unlike classical Matching Pursuit, OMP recomputes all selected coefficients
#' at each iteration by solving a least-squares problem, which generally
#' yields more accurate sparse approximations for a given number of atoms.
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
#' @param fit_intercept
#' Logical; if \code{TRUE}, the signal and dictionary atoms
#' are centered before decomposition and an intercept term is estimated.
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
#' \item{original_signal}{The original signal reconstructed as a vector
#'   (including intercept if \code{fit_intercept = TRUE}).}
#' \item{reconstruction}{The OMP approximation of the signal including intercept
#'   (if applicable).}
#' \item{coef}{Numeric vector of estimated coefficients for selected atoms.}
#' \item{energy}{Energy contribution of selected atoms, computed as
#'   \code{coef^2 * colSums(selected_atoms^2)}.}
#' \item{intercept}{Estimated intercept term (0 if \code{fit_intercept = FALSE}).}
#' \item{support}{Integer vector of selected atom indices.}
#' \item{residual}{Final residual vector.}
#' \item{n_iters}{Number of iterations performed by the algorithm.}
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
#' fit <- omp_core(
#'   dictionary = dictionary,
#'   signal = signal,
#'   channel = 3,
#'   n_nonzero_coefs = 3,
#'   verbose = TRUE
#' )
#'
#' fit$coef
#' fit$support
#'
#' # More realistic example, see omp_execute() examples.
#'
#'
omp_core <- function(
    dictionary,
    signal,
    channel = NULL,
    n_nonzero_coefs = NULL,
    tol = NULL,
    normalize = TRUE,
    fit_intercept = TRUE,
    verbose = FALSE
) {

  # Preprocessing
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

  n <- nrow(D)
  p <- ncol(D)

  if (is.vector(signal) || is.data.frame(signal) || is.matrix(signal)) {
    signal <- as.matrix(signal)
  } else {
    stop("'signal' must be a matrix or convertible to a matrix.")
  }

  sig <- signal[, channel]
  sig <- as.numeric(sig)
  total_energy <- sum(sig^2)

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

  # Intercept handling
  sig_mean <- rep(0)
  D_mean <- rep(0, p)

  if (fit_intercept) {
    sig_mean <- mean(sig)
    sig <- sig - sig_mean
    D_mean <- colMeans(D)
    D <- sweep(D, 2, D_mean)
  }

  # Normalization
  norms <- rep(1, p)

  if (normalize) {
    norms <- sqrt(colSums(D^2))
    for (j in 1:p) {
      if (norms[j] > 0) {
        D[, j] <- D[, j] / norms[j]
      }
    }
  }

  # Pre-compute Dtsig only
  Dtsig <- crossprod(D, sig)

  # Outputs
  support <- integer(0)
  coefs <- rep(0, p)
  residual <- sig
  L <- NULL

  relative_residual_energy <- numeric(max_iter + 1)
  relative_residual_energy[1] <- sum(residual^2) / total_energy

  # Main OMP loop
  for (k in 1:max_iter) {

    # 1. Select atom
    corr <- as.vector(crossprod(D, residual))

    if (length(support) > 0) {
      corr[support] <- 0
    }

    j <- which.max(abs(corr))
    support <- c(support, j)

    if (verbose) message("iteration: ", k, ", selected atom: ", j)

    # 2. Update Cholesky factor
    if (k == 1) {
      # atom norm
      dj_norm_sq <- sum(D[, j]^2)
      L <- matrix(sqrt(dj_norm_sq), nrow = 1)
    } else {
      prev_support <- support[-length(support)]
      # lazy Gram computation
      w <- crossprod(D[, prev_support, drop = FALSE], D[, j])
      # Solve L v = w
      v <- forwardsolve(L, w)
      dj_norm_sq <- sum(D[, j]^2)
      alpha <- dj_norm_sq - sum(v^2)

      if (alpha <= 1e-12) {
        warning(paste("Near linear dependence detected at iteration", k))
        break
      }

      diag_val <- sqrt(alpha)
      L <- rbind(cbind(L, rep(0, nrow(L))), c(v, diag_val))
    }

    # 3. Solve least square
    b <- Dtsig[support]
    z <- forwardsolve(L, b)
    x_active <- backsolve(t(L), z)
    x_active <- as.numeric(x_active)

    # 4. Build full coefficient vector
    coefs <- rep(0, p)
    coefs[support] <- x_active

    # 5. Update residual
    residual <- sig - D[, support, drop = FALSE] %*% x_active

    # 6. Stopping criterion
    # tol = maximum squared residual norm ||r||^2
    residual_sq_norm <- sum(residual^2)
    relative_error <- residual_sq_norm / total_energy
    relative_residual_energy[k + 1] <- relative_error

    if (!is.null(tol)) {
      if (verbose) message("relative residual energy: ", signif(relative_residual_energy[k + 1], 6))
      if (relative_error <= tol) {
        message("Convergence achieved in iteration: ", k, "\n")
        break
      }
    }
  }

  # intercept recovery
  intercept <- 0

  if (fit_intercept) {
    intercept <- sig_mean - sum(D_mean * coefs)
  }

  # Selected topk atoms
  if (inherits(dictionary, "topk")) {
    frequency <- dictionary$frequency[support, channel]
    phase <- dictionary$phase[support, channel]
    scale <- dictionary$scale[support, channel]
    position <- dictionary$position[support, channel]
  }

  selected_atoms <- D[, support]
  coefs_selected  <- coefs[support]
  energy <- coefs_selected^2 * colSums(selected_atoms^2)

  # Output
  if (inherits(dictionary, "topk")) {
    list(
      gabors = selected_atoms,
      original_signal = sig + intercept,
      reconstruction = as.vector(selected_atoms %*% coefs_selected + intercept),
      coefs = coefs_selected,
      energy = energy,
      intercept = intercept,
      support = support,
      residual = as.vector(residual),
      relative_residual_energy = relative_residual_energy[1:(k+1)],
      n_iters = k,
      frequency = frequency,
      phase = phase,
      scale = scale,
      position = position
    )
  } else {
    list(
      gabors = selected_atoms,
      original_signal = sig + intercept,
      reconstruction = as.vector(selected_atoms %*% coefs_selected + intercept),
      coefs = coefs_selected,
      energy = energy,
      intercept = intercept,
      support = support,
      residual = as.vector(residual),
      relative_residual_energy = relative_residual_energy[1:(k+1)],
      n_iters = k
    )
  }
}





