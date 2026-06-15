#' Run the complete OMP decomposition pipeline
#'
#' Executes the full Orthogonal Matching Pursuit (OMP) workflow:
#' reads a signal from a CSV file, loads a dictionary definition from an
#' XML file, selects the most relevant atoms, and performs sparse signal
#' decomposition using OMP.
#'
#' @param sig_file
#' Path to a CSV file containing the signal data.
#' See \code{read_csv_signals()} function.
#'
#' @param col_names_in_csv
#' Logical. Indicates whether the CSV file contains column names in the first row.
#' See \code{read_csv_signals()} function.
#'
#' @param xml_file
#' Path to an XML file defining the dictionary atoms.
#' See \code{read_dict()} function.
#'
#' @param topk
#' Integer. Number of atoms with the highest correlation to the
#' signal retained for OMP fitting.
#' See \code{topk_atoms()} function.
#'
#' @param n_nonzero_coefs
#' Integer. Maximum number of non-zero coefficients  to include in the sparse decomposition.
#'
#' @param tol
#' Optional numeric tolerance for the stopping criterion. If code{NULL}, the
#' algorithm stops after selecting \code{n_nonzero_coefs} atoms.
#'
#' @param normalize
#' Logical. If \code{TRUE}, dictionary atoms are normalized  before fitting.
#'
#' @param fit_intercept
#' Logical. If  \code{TRUE}, an intercept term is included in the model.
#'
#' @param verbose
#' Logical. If  \code{TRUE}, progress information is printed to  the console.
#'
#' @return An object of class \code{"mp"} containing:
#'
#' \item{atoms}{A data frame describing the selected atoms.}
#' \item{original_signal}{Matrix containing the original signal(s).}
#' \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#' \item{gabors}{List of matrices containing selected atoms for each channel.}
#' \item{t}{Time vector corresponding to signal samples.}
#' \item{sf}{Sampling frequency.}
#'
#' @export
#'
#' @examples
#' sig_file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#' xml_file <- system.file("extdata", "sample3_dict.xml", package = "MatchingPursuit")
#'
#' out <- run_omp_pipeline(
#'   sig_file = sig_file,
#'   col_names_in_csv = TRUE,
#'   xml_file = xml_file,
#'   topk = 5000,
#'   n_nonzero_coefs = 50,
#'   verbose = TRUE
#' )
#'
#' plot(out, channel = 3)
#'
run_omp_pipeline <- function(
    sig_file,
    col_names_in_csv,
    xml_file,
    topk,
    n_nonzero_coefs,
    tol = NULL,
    normalize = TRUE,
    fit_intercept = TRUE,
    verbose = FALSE) {

  sig <- read_csv_signals(
    file = sig_file,
    col_names_in_csv = col_names_in_csv
  )

  sf <- sig$sampling_frequency
  signal <- sig$signal
  duration <- nrow(signal) / sf

  atoms_dict <- read_dict(
    xml_file = xml_file,
    sf = sf,
    duration = duration,
    verbose = verbose
  )

  topk_dict <- topk_atoms(
    atoms_dict = atoms_dict,
    sig = signal,
    sf = sf,
    topk = topk,
    verbose = verbose
  )

  if (verbose) cat("\n")

  fit <- omp_execute(
    dictionary = topk_dict,
    signal = signal,
    sf = sf,
    n_nonzero_coefs = n_nonzero_coefs,
    tol = tol,
    normalize = normalize,
    fit_intercept = fit_intercept,
    verbose = verbose
  )
}
