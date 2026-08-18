#' Run an MP or OMP decomposition pipeline
#'
#' @description
#' Runs a higher-level Matching Pursuit (MP) or Orthogonal Matching Pursuit
#' (OMP) decomposition workflow for signals stored in CSV format. The function:
#' (1) imports the signal, (2) loads a Gabor dictionary definition from an XML
#' file, (3) selects the most relevant candidate atoms, and (4) performs sparse
#' decomposition using the selected algorithm.
#'
#' Signal-specific preprocessing, such as filtering, resampling, or EEG montage
#' construction, should be performed separately before using this function.
#'
#' @param mode Character string, either \code{"mp"} or \code{"omp"}, specifying
#'   the decomposition algorithm.
#'
#' @param sig_file Path to a CSV file containing the signal data.
#'
#' @param col_names_in_csv Logical; indicates whether the CSV file contains
#'   column names in the first row. See \code{read_csv_signals()}.
#'
#' @param xml_file Path to an XML file defining the Gabor dictionary.
#'   See \code{read_gabor_dict()}.
#'
#' @param topk Positive integer specifying the number of candidate atoms with
#'   the highest similarity to the signal retained for MP/OMP decomposition.
#'   See \code{topk_atoms()}.
#'
#' @param n_nonzero_coefs Maximum number of non-zero coefficients in the sparse
#'   decomposition. If \code{tol = NULL}, the algorithm stops after selecting
#'   at most this number of atoms.
#'
#' @param tol Optional numeric tolerance for the stopping criterion. If
#'   specified, the algorithm stops when the residual energy falls below this
#'   value and \code{n_nonzero_coefs} is ignored.
#'
#' @param normalize Logical; if \code{TRUE}, dictionary atoms are normalized to
#'   unit L2 norm before decomposition.
#'
#' @param fit_intercept Logical; if \code{TRUE}, an intercept term is included
#'   in the model. Used only when \code{mode = "omp"}.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed to
#'   the console.
#'
#' @return An object of class \code{mp} containing the decomposition results.
#'   See \code{mp_omp_execute()}.
#'
#' @export
#'
#' @seealso
#' \code{\link{omp_core}},
#' \code{\link{mp_core}},
#' \code{\link{mp_omp_execute}},
#' \code{\link{topk_atoms}},
#' \code{\link{read_gabor_dict}},
#'
#' @examples
#' sig_file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#' xml_file <- system.file("extdata", "sample3.xml", package = "MatchingPursuit")
#'
#' out_mp <- mp_omp_pipeline(
#'   mode = "mp",
#'   sig_file = sig_file,
#'   col_names_in_csv = TRUE,
#'   xml_file = xml_file,
#'   topk = 5000,
#'   n_nonzero_coefs = 50,
#'   verbose = TRUE
#' )
#'
#' out_omp <- mp_omp_pipeline(
#'   mode = "omp",
#'   sig_file = sig_file,
#'   col_names_in_csv = TRUE,
#'   xml_file = xml_file,
#'   topk = 5000,
#'   n_nonzero_coefs = 50,
#'   verbose = TRUE
#' )
#'
#' plot(out_mp, channel = 2)
#' plot(out_omp, channel = 2)
#'
mp_omp_pipeline <- function(
    mode = NULL,
    sig_file,
    col_names_in_csv = FALSE,
    xml_file,
    topk,
    n_nonzero_coefs = NULL,
    tol = NULL,
    normalize = TRUE,
    fit_intercept = TRUE,
    verbose = FALSE) {

  if (is.null(mode)) {
    stop("'mode' must be specified.")
  }

  if (!mode %in% c("mp", "omp")) {
    stop("'mode' must be either 'mp' or 'omp'.")
  }

  if (!file.exists(sig_file)) {
    stop("Signal file does not exist: ", sig_file)
  }

  if (!file.exists(xml_file)) {
    stop("Dictionary XML file does not exist: ", xml_file)
  }

  if (!is.numeric(topk) ||
      length(topk) != 1L ||
      !is.finite(topk) ||
      topk < 1 ||
      topk != as.integer(topk)) {
    stop("'topk' must be a positive integer.")
  }

  sig <- read_csv_signals(
    file = sig_file,
    col_names_in_csv = col_names_in_csv
  )

  sampling_frequency <- sig$sampling_frequency
  signal <- sig$signal
  duration <- nrow(signal) / sampling_frequency

  atoms_dict <- read_gabor_dict(
    xml_file = xml_file,
    sampling_frequency = sampling_frequency,
    duration = duration,
    verbose = verbose
  )

  topk_dict <- topk_atoms(
    atoms_dict = atoms_dict,
    signal = sig,
    topk = topk,
    verbose = verbose
  )

  if (mode == "omp") {
  fit <- mp_omp_execute(
    mode = "omp",
    dictionary = topk_dict,
    signal = sig,
    n_nonzero_coefs = n_nonzero_coefs,
    tol = tol,
    normalize = normalize,
    fit_intercept = fit_intercept,
    verbose = verbose)
  }

  if (mode == "mp") {
    fit <- mp_omp_execute(
      mode = "mp",
      dictionary = topk_dict,
      signal = sig,
      n_nonzero_coefs = n_nonzero_coefs,
      tol = tol,
      normalize = normalize,
      verbose = verbose)
  }

  return(fit)

}
