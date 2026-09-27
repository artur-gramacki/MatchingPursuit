#' Matching Pursuit (MP) or Orthogonal Matching Pursuit (OMP) decomposition
#' for multi-channel signals
#'
#' Performs sparse signal decomposition using either the Matching Pursuit (MP) or
#' Orthogonal Matching Pursuit (OMP) algorithm, as specified by the \code{mode}
#' parameter. The decomposition is performed independently for each signal channel
#' using a dictionary of candidate atoms selected by \code{topk_gabor_atoms()}.
#'
#' The returned object is of class \code{"mp"} and can be visualized using
#' \code{plot()} and \code{tf_map()}.
#'
#' @param signal An object of class \code{sig} returned by \code{read_csv_signals()},
#' an object of class \code{edf} returned by \code{read_edf_signals()},
#' or an object of class \code{wfdb} returned by \code{read_wfdb_signals()}.
#'
#' @param mode \code{"omp"} or \code{"mp"}. Specifies the algorithm to use
#' for signal decomposition.
#'
#' @param dictionary Character string specifying the path to an XML file
#' containing the dictionary specification. It can be generated using
#' \code{generate_xml_dict()} or prepared manually by the user. If \code{NULL},
#' the XML file is generated internally by the function.
#'
#' @param topk Positive integer specifying the number of highest-ranked candidate
#' atoms retained for each signal channel after ranking by their match to the
#' signal. If \code{NULL}, the top 10\% of candidate atoms are retained for
#' each channel.
#'
#' @param full_atoms_in_signal Logical. If \code{TRUE}, only candidate atoms
#' whose complete support lies within the signal are included in the dictionary.
#' If \code{FALSE}, candidate atom supports may extend beyond the signal
#' boundaries.
#'
#' @param n_nonzero_coefs Maximum number of atoms selected during the decomposition
#' for each signal channel.
#'
#' @param tol Optional stopping tolerance defined as the maximum allowed
#' relative residual energy. If specified, it overrides \code{n_nonzero_coefs}.
#'
#' @param sparse
#' Logical; passed to \code{gabor_atoms_matrix()} when constructing the
#' channel-specific matrix of selected Gabor atoms. If \code{TRUE}, the atom
#' matrix is created directly as a sparse matrix; if \code{FALSE}, a dense
#' matrix is created. Sparse storage can substantially reduce memory use and
#' improve MP/OMP performance because individual Gabor atoms have localized
#' support and are zero outside their support intervals. Defaults to \code{TRUE}.
#'
#' @param verbose Logical; if \code{TRUE}, progress information is printed
#' during processing.
#'
#' @return An object of class \code{"mp"} containing:
#'
#' \item{atoms}{A data frame describing the selected atoms.}
#' \item{signal}{Matrix containing the original signal(s).}
#' \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#' \item{selected_atoms}{List of matrices containing selected unit-L2-normalized
#'   atoms for each channel.}
#' \item{time}{Time vector corresponding to signal samples.}
#' \item{sampling_frequency}{Sampling frequency.}
#'
#' The \code{atoms} data frame contains:
#'
#' \itemize{
#' \item \code{channel_id} — signal channel identifier,
#' \item \code{atom_number} — atom index within the channel,
#' \item \code{energy} — atom energy contribution,
#' \item \code{envelope} — envelope type,
#' \item \code{frequency} — atom frequency (Hz),
#' \item \code{phase} — atom phase (radians),
#' \item \code{scale} — atom scale (seconds),
#' \item \code{position} — atom centre position (seconds).
#' }
#'
#' @details
#' The XML dictionary specification is first processed by
#' \code{read_gabor_dict()}. For each signal channel,
#' \code{topk_gabor_atoms()} ranks the candidate Gabor atoms and retains the
#' channel-specific top-ranked subset together with the parameters required to
#' reconstruct those atoms. \code{gabor_atoms_matrix()} then materializes the
#' selected atoms as a dense or sparse matrix, according to \code{sparse}.
#' This matrix is passed to \code{mp_core()} or \code{omp_core()} for the
#' actual decomposition of that channel.
#'
#' The results from all channels are combined into an object of class
#' \code{"mp"}, which can be visualized using \code{plot()} and
#' \code{tf_map()}.
#'
#' @seealso
#' \code{\link{read_gabor_dict}},
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{gabor_atoms_matrix}},
#' \code{\link{generate_xml_dict}},
#' \code{\link{omp_core}},
#' \code{\link{mp_core}}
#'
#' @export
#'
#' @examples
#' # +-------------------------------------------------------------+
#' # | Read signal                                                 |
#' # +-------------------------------------------------------------+
#' file <- system.file(
#'   "extdata",
#'   "sample1.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' signal <- read_csv_signals(
#'   file,
#'   col_names_in_csv = FALSE
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Run Matching Pursuit (MP-R backend)                         |
#' # +-------------------------------------------------------------+
#' # Set mode = "omp" to run Orthogonal Matching Pursuit (OMP-R backend).
#' #
#' # topk is set to a relatively small value to reduce computation time.
#' # If this parameter is omitted, the function sets topk by default to 10%
#' # of the total number of atoms in the dictionary.
#' #
#' # In practical applications, n_nonzero_coefs is typically set to a larger
#' # value, e.g. 50.
#' #
#' fit_mp <- mp_omp_execute(
#'   mode = "mp",
#'   signal = signal,
#'   n_nonzero_coefs = 50,
#'   topk = 10000,
#'   verbose = TRUE
#' )
#'
#' plot(fit_mp, freq_divide = 4)
#'
#' # ### NOTE ###
#' # Additional examples are provided below for illustration.
#' # They are commented out because they may take longer to run.
#'
#' # The 'full_atoms_in_signal' argument restricts the
#' # decomposition to atoms fully contained within the analyzed
#' # signal. Compare the two time-frequency maps obtained with
#' # and without this restriction.
#'
#' # fit_mp <- mp_omp_execute(
#' #   mode = "mp",
#' #   signal = signal,
#' #   full_atoms_in_signal = TRUE,
#' #   n_nonzero_coefs = 50,
#' #   topk = 10000,
#' #   verbose = TRUE
#' # )

#' # plot(fit_mp, freq_divide = 4)
#'
#' # +-------------------------------------------------------------+
#' # | Run Orthogonal Matching Pursuit (OMP-R backend)             |
#' # +-------------------------------------------------------------+
#' # fit_omp <- mp_omp_execute(
#' #   mode = "omp",
#' #   signal = signal,
#' #   n_nonzero_coefs = 50,
#' #   verbose = TRUE
#' # )
#'
#' # plot(fit_omp, freq_divide = 4)
#'
#' # +-------------------------------------------------------------+
#' # | Use an external XML dictionary specification                |
#' # +-------------------------------------------------------------+
#' # xml_file <- system.file(
#' #   "extdata",
#' #   "sample1.xml",
#' #    package = "MatchingPursuit"
#' # )
#'
#' # fit_mp_xml <- mp_omp_execute(
#' #   mode = "mp",
#' #   signal = signal,
#' #   dictionary = xml_file,
#' #   n_nonzero_coefs = 50,
#' #   verbose = TRUE
#' # )
#'
#' # plot(fit_mp_xml, freq_divide = 4)
#'
mp_omp_execute <- function (
    signal,
    mode = NULL,
    dictionary = NULL,
    topk = NULL,
    full_atoms_in_signal = FALSE,
    n_nonzero_coefs = NULL,
    tol = NULL,
    sparse = TRUE,
    verbose = FALSE
) {

  if (is.null(mode)) {
    stop("'mode' must be specified.")
  }

  if (!mode %in% c("mp", "omp")) {
    stop("'mode' must be either 'mp' or 'omp'.")
  }

  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig <- as.matrix(signal$signal)
  sampling_frequency <- signal$sampling_frequency
  signal_length <- nrow(signal$signal)
  duration <- nrow(signal$signal) / sampling_frequency

  if (is.null(dictionary)) {
    xml_file <- tempfile(fileext = ".xml")
    generate_xml_dict(N = signal_length, file = xml_file)
  } else {
    xml_file <- dictionary
  }

  atoms_dict <- read_gabor_dict(
    xml_file = xml_file,
    sampling_frequency = sampling_frequency,
    duration = duration,
    full_atoms_in_signal = full_atoms_in_signal,
    verbose = verbose
  )

  channel_id <- c()
  atom_number <- c()
  energy <- c()
  envelope <- c()
  frequency <- c()
  phase <- c()
  scale <- c()
  position <- c()
  selected_atoms <-list()

  results <- vector("list", ncol(sig))

  topk_atoms <- topk_gabor_atoms(
    atoms_dict = atoms_dict,
    signal = signal,
    topk = topk,
    verbose = verbose
  )

  for (ch in seq_len(ncol(sig))) {

    D <- gabor_atoms_matrix(
      x = topk_atoms,
      channel = ch,
      sparse = sparse,
      verbose = verbose
    )

    if (mode == "omp") {
      res <- omp_core(
        dictionary = D,
        signal = sig,
        channel = ch,
        tol = tol,
        n_nonzero_coefs = n_nonzero_coefs,
        sparse = sparse,
        verbose = verbose
      )
    }

    if (mode == "mp") {
      res <- mp_core(
        dictionary = D,
        signal = sig,
        channel = ch,
        tol = tol,
        n_nonzero_coefs = n_nonzero_coefs,
        sparse = sparse,
        verbose = verbose
      )
    }

    results[[ch]] <- res

    num_atoms <- ncol(results[[ch]]$selected_atoms)
    support <- results[[ch]]$support

    fr <- topk_atoms$frequency[support, ch]
    ph <- topk_atoms$phase[support, ch]
    sc <- topk_atoms$scale[support, ch]
    po <- topk_atoms$position[support, ch]

    i1 <- rep(ch, num_atoms)
    channel_id <- c(channel_id, i1)

    i2 <- seq(1, num_atoms)
    atom_number <- c(atom_number, i2)

    i3 <- results[[ch]]$energy
    energy <- c(energy, i3)

    i4 <- rep("gauss", num_atoms)
    envelope <-c(envelope, i4)

    i5 <- fr
    frequency <-c(frequency, i5)

    i6 <- ph
    phase <-c(phase, i6)

    i7 <- sc
    scale <-c(scale, i7)

    i8 <- po
    position <-c(position, i8)

    message("mp_omp_execute(): method = \"", mode, "\", channel = ", ch, " Successfully processed.")
  }

  n_channels <- length(results)
  n_samples <- nrow(results[[1]]$selected_atoms)
  time <- seq(0, (n_samples - 1) / sampling_frequency, by = 1 / sampling_frequency)

  original_signal <- matrix(NA, nrow = n_samples, ncol = n_channels)
  reconstruction <- matrix(NA, nrow = n_samples, ncol = n_channels)

  for (r in 1:n_channels) {
    selected_atoms[[r]] <- results[[r]]$selected_atoms
    reconstruction[, r] <- results[[r]]$selected_atoms %*% results[[r]]$coefs
    original_signal[, r] <- results[[r]]$signal
  }

  out <- list()
  out$atoms <- data.frame(
    channel_id,
    atom_number,
    energy,
    envelope,
    frequency,
    phase,
    scale,
    position
  )
  out$signal <- original_signal
  out$reconstruction <- reconstruction
  out$selected_atoms <- selected_atoms
  out$time <- time
  out$sampling_frequency <- sampling_frequency
  class(out) <- "mp"

  return(out)
}
