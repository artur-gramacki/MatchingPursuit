#' Reference implementation of Orthogonal Matching Pursuit (OMP)
#'
#' A straightforward reference implementation of the Orthogonal Matching
#' Pursuit (OMP) algorithm that closely follows its mathematical formulation.
#' The function is intended for reference and illustrative purposes and does
#' not use computational optimizations. The least-squares problem is solved
#' explicitly using the normal-equation formula.
#'
#' This implementation is intended for small illustrative examples rather
#' than large-scale computations. In particular, when \code{verbose = TRUE},
#' detailed information is printed at every iteration, including residual
#' and reconstruction vectors. For large signals or a large number of
#' iterations, this may produce a substantial amount of console output.
#'
#' This implementation operates on a single signal represented by a numeric
#' vector. The \code{signal} argument must therefore be a numeric vector,
#' not a matrix containing multiple signals or channels.
#'
#' Because the least-squares problem is solved explicitly through the normal
#' equations, the implementation may fail for singular or nearly singular
#' selected subdictionaries and is not intended for numerically demanding
#' applications.
#'
#' @param dictionary
#' A dictionary of atoms. Can be a numeric vector, matrix, or data frame.
#' Atoms are assumed to be stored in columns.
#' Dictionary atoms are internally normalized to unit L2 norm before decomposition.
#' Therefore, atom selection is invariant to arbitrary scaling of dictionary columns.
#'
#' @param signal A numeric vector.
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
#' @param verbose Logical. If \code{TRUE}, information about the progress of
#'   the algorithm and the orthogonality of the residual is printed.
#'
#' @return A list containing:
#'   \item{selected_atoms}{Indices of the atoms selected by OMP.}
#'
#'   \item{coefficients_normalized_dict}{Coefficient vector corresponding
#'     to the normalized working dictionary.}
#'
#' \item{coefficients_original_dict}{Coefficient vector corresponding
#'   to the original (unnormalized) dictionary.}
#'
#'   \item{reconstruction_normalized_dict}{Signal reconstruction obtained
#'     using the normalized working dictionary.}
#'
#'   \item{reconstruction_original_dict}{Signal reconstruction obtained
#'     using the original dictionary.}
#'
#'   \item{residual_normalized_dict}{Residual corresponding to the
#'     reconstruction based on the normalized working dictionary.}
#'
#'   \item{residual_original_dict}{Residual corresponding to the
#'     reconstruction based on the original dictionary.}
#'
#'   \item{normalized_reconstruction_error_normalized_dict}{
#'     Normalized reconstruction error (NRE) for the normalized working
#'     dictionary, defined as \eqn{\|y - \hat{y}\|_2 / \|y\|_2}.}
#'
#'   \item{normalized_reconstruction_error_original_dict}{
#'     Normalized reconstruction error (NRE) for the original dictionary,
#'     defined as \eqn{\|y - \hat{y}\|_2 / \|y\|_2}.}
#'
#'   \item{orthogonality}{A list containing, for each OMP iteration,
#'     the inner products between the current residual and all atoms
#'     selected up to that iteration. Values should be numerically close
#'     to zero.}
#'
#'   \item{residual_energy}{Residual energy after each OMP iteration,
#'     defined as \eqn{\|r_k\|_2^2}.}
#'
#'   \item{iterations}{Number of OMP iterations performed.}
#'
#' @details
#' Dictionary atoms are internally normalized to unit Euclidean norm before
#' decomposition. The returned coefficients are provided both for the
#' normalized working dictionary and for the original dictionary scaling.
#'
#' @export
#'
#' @seealso
#' \code{\link{omp_core}},
#' \code{\link{mp_omp_execute}}
#'
#' @examples
#'
#' dictionary <- matrix(
#' c(
#'   1.0,  0.9,  0.1,  1.0, -0.2,  0.3,  0.7, -0.5,  1.2,  0.4,
#'   0.2,  1.0,  0.8, -0.3,  1.0, -0.6,  0.5,  0.9, -0.1,  0.8,
#'   0.0,  0.1,  1.0,  0.5,  0.7,  1.1, -0.4,  0.2,  0.6, -0.7,
#'   0.9, -0.2,  0.4,  1.3,  0.1,  0.0,  0.8, -0.9,  0.5,  1.0,
#'   -0.3,  0.6,  1.1, -0.4,  0.2,  0.7, -0.8,  1.0,  0.3,  0.9),
#' nrow = 5, byrow = TRUE
#' )
#'
#' signal <- c(4, 2, 3, 5, 1)
#'
#' out <- omp_reference(
#'   dictionary = dictionary,
#'   signal = signal,
#'   n_nonzero_coefs = 3,
#'   verbose = TRUE
#' )
#'
#' out
#'
omp_reference <- function(
    dictionary,
    signal,
    n_nonzero_coefs = NULL,
    tol = NULL,
    verbose = FALSE) {

  # ----------------------------------------------------------+--+
  # Basic checks
  # ------------------------------------------------------------+
  D <- as.matrix(dictionary)

  if (!is.numeric(signal) || !is.null(dim(signal))) {
    stop("'signal' must be a numeric vector representing a single signal.")
  }

  y <- as.numeric(signal)

  if (any(!is.finite(y))) {
    stop("'signal' must contain only finite numeric values.")
  }

  if (sum(y^2) == 0) {
    stop("'signal' must have non-zero Euclidean norm.")
  }

  if (nrow(D) != length(y)) {
    stop("Number of rows in 'dictionary' must equal length of 'signal'.")
  }

  if (is.null(n_nonzero_coefs) && is.null(tol)) {
    n_nonzero_coefs <- max(1L, floor(0.1 * ncol(D)))
  }

  if (!is.null(n_nonzero_coefs)) {
    if (length(n_nonzero_coefs) != 1L ||
        !is.numeric(n_nonzero_coefs) ||
        !is.finite(n_nonzero_coefs) ||
        n_nonzero_coefs <= 0 ||
        n_nonzero_coefs != floor(n_nonzero_coefs)) {
      stop("'n_nonzero_coefs' must be a positive integer.")
    }

    if (n_nonzero_coefs > ncol(D)) {
      stop("'n_nonzero_coefs' cannot exceed the number of dictionary atoms.")
    }
  }

  if (!is.null(tol)) {
    if (length(tol) != 1L ||
        !is.numeric(tol) ||
        !is.finite(tol) ||
        tol < 0 ||
        tol > 1) {
      stop("'tol' must be a single finite numeric value between 0 and 1.")
    }
  }

  if (!is.numeric(D) || any(!is.finite(D))) {
    stop("'dictionary' must be a finite numeric matrix.")
  }

  atom_norms <- sqrt(colSums(D^2))

  if (any(!is.finite(atom_norms)) || any(atom_norms <= .Machine$double.eps)) {
    stop("Dictionary contains non-finite or zero-norm atoms.")
  }

  D <- sweep(D, MARGIN = 2, STATS = atom_norms, FUN = "/")

  # ------------------------------------------------------------+
  # Initial values
  # ------------------------------------------------------------+
  residual <- y
  selected <- integer(0)
  coefficients_selected <- numeric(0)
  reconstruction <- rep(0, length(y))
  iteration <- 0

  # Store the orthogonality check from each iteration
  orthogonality <- list()

  # Store the residual energy for each iteration
  residual_energy <-numeric(0)

  # ------------------------------------------------------------+
  # OMP loop
  # ------------------------------------------------------------+
  repeat {
    iteration <- iteration + 1

    # ----------------------------------------------------------+
    # 1. Compute inner products between residual and all atoms
    # ----------------------------------------------------------+
    projections <- as.numeric(t(D) %*% residual)

    # Do not select the same atom twice
    if (length(selected) > 0) {
      projections[selected] <- 0
    }

    # ----------------------------------------------------------+
    # 2. Select the atom with the largest absolute projection
    # ----------------------------------------------------------+
    # Find the largest absolute projection among the remaining atoms.
    max_projection <- max(abs(projections))

    # If all remaining atoms are numerically orthogonal to the current residual,
    #no further atom can reduce the residual, so the OMP procedure terminates.
    if (max_projection <= .Machine$double.eps) break

    new_atom <- which.max(abs(projections))
    selected <- c(selected, new_atom)

    # ----------------------------------------------------------+
    # 3. Construct matrix of all selected atoms
    # ----------------------------------------------------------+
    D_selected <- D[, selected, drop = FALSE]

    # ----------------------------------------------------------+
    # 4. Explicit least-squares solution, written directly from
    # the mathematical formula:
    #
    # beta = (D_S^T D_S)^(-1) D_S^T y
    # ----------------------------------------------------------+
    coefficients_selected <- solve(t(D_selected) %*% D_selected) %*% t(D_selected) %*% y
    coefficients_selected <- as.numeric(coefficients_selected)

    # ----------------------------------------------------------+
    # 5. Reconstruct signal using all selected atoms
    # ----------------------------------------------------------+
    reconstruction <- as.numeric(D_selected %*% coefficients_selected)

    # ----------------------------------------------------------+
    # 6. Update residual
    # ----------------------------------------------------------+
    residual <- y - reconstruction

    residual_energy[iteration] <- sum(residual^2)

    # ----------------------------------------------------------+
    # 7. Check orthogonality of the residual
    #
    # For the least-squares solution:
    #
    #                 D_S^T r = 0
    #
    # Therefore the residual is orthogonal to every atom
    # selected so far.
    # ----------------------------------------------------------+
    residual_projections <- as.numeric(t(D_selected) %*% residual)
    orthogonality[[iteration]] <- residual_projections

    if (verbose) {
      cat(
        "\nIteration:", iteration,
        "  selected atom:", new_atom, "\n"
      )

      cat("Residual projections onto selected atoms:\n")
      print(residual_projections)

      cat(
        "Maximum absolute projection:", max(abs(residual_projections)), "\n"
      )
    }

    # ----------------------------------------------------------+
    # Progress information
    # ----------------------------------------------------------+
    if (verbose) {
      cat(
        "Residual energy: ",
        signif(residual_energy[iteration], 6),
        "\nResidual vector = [", paste(signif(residual, 6), collapse = ", "), "]",
        "\nReconstruction vector = [", paste(signif(reconstruction, 6), collapse = ", "), "]\n"
      )
    }

    # ----------------------------------------------------------+
    # 8. Stopping criteria
    # ----------------------------------------------------------+
    if (residual_energy[iteration] <=
        .Machine$double.eps * sum(y^2)) {
      break
    }

    relative_residual_energy <-
      residual_energy[iteration] / sum(y^2)

    if (!is.null(tol)) {
      if (relative_residual_energy <= tol) {
        break
      }
    } else {
      if (iteration >= n_nonzero_coefs) {
        break
      }
    }
  }

  # ------------------------------------------------------------+
  # Convert coefficients back to original dictionary scaling
  # ------------------------------------------------------------+
  # Coefficients for the normalized (working) dictionary
  coefficients_normalized_dict <- rep(0, ncol(D))
  coefficients_normalized_dict[selected] <- coefficients_selected

  # Coefficients for the original (unnormalized) dictionary
  coefficients_original_dict <- rep(0, ncol(D))
  coefficients_original_dict[selected] <-  coefficients_selected / atom_norms[selected]

  # Reconstruction using the original dictionary
  reconstruction_original_dict <- as.numeric(as.matrix(dictionary) %*% coefficients_original_dict)
  residual_original_dict <- y - reconstruction_original_dict

  # Reconstruction using the normalized dictionary
  reconstruction_normalized_dict <- as.numeric(D %*% coefficients_normalized_dict)
  residual_normalized_dict <- y - reconstruction_normalized_dict

  # Normalized reconstruction error: ||y - y_hat||_2 / ||y||_2
  normalized_reconstruction_error_normalized_dict <-
    sqrt(sum(residual_normalized_dict^2)) / sqrt(sum(y^2))

  normalized_reconstruction_error_original_dict <-
    sqrt(sum(residual_original_dict^2)) / sqrt(sum(y^2))

  # The following relationships should hold:
  # reconstruction_original_dict ~= reconstruction_normalized_dict
  # and:
  # residual_original_dict ~= residual_normalized_dict

  # ------------------------------------------------------------+
  # Output
  # ------------------------------------------------------------+
  list(
    selected_atoms = selected,

    coefficients_normalized_dict = coefficients_normalized_dict,
    coefficients_original_dict = coefficients_original_dict,

    reconstruction_normalized_dict = reconstruction_normalized_dict,
    reconstruction_original_dict = reconstruction_original_dict,

    residual_normalized_dict = residual_normalized_dict,
    residual_original_dict = residual_original_dict,

    # Since both reconstructions should be identical, the corresponding
    # reconstruction errors should also be practically identical.
    normalized_reconstruction_error_normalized_dict = normalized_reconstruction_error_normalized_dict,
    normalized_reconstruction_error_original_dict = normalized_reconstruction_error_original_dict,

    orthogonality = orthogonality,
    residual_energy = residual_energy,

    iterations = iteration
  )
}
