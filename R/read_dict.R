#' Read dictionary of Gabor atoms from XML file
#'
#' The function parses an XML file describing a multiscale Gabor dictionary.
#'
#' @param xml_file
#' Path to the XML file containing the dictionary definition.
#'
#' @param sf
#' Sampling frequency (in Hz) of the signal associated with the  dictionary.
#'
#' @param duration
#' Duration of the signal (in seconds) used to determine the number
#' of valid time positions.
#'
#' @param verbose
#' Logical; if \code{TRUE}, prints progress information about  parsed blocks
#' and generated atoms.
#'
#' @return A character matrix where each row describes a Gabor atom with the following columns:
#'
#' \item{block}{Block identifier from the XML file.}
#' \item{time_sample}{Time position of the atom (in samples).}
#' \item{time_sec}{Time position of the atom (in seconds).}
#' \item{freq_bin}{Frequency bin index.}
#' \item{freq_hz}{Frequency in Hertz.}
#' \item{window_len}{Window length used for the atom.}
#' \item{fft_size}{FFT size used for the atom.}
#'
#' @details
#' Each \code{<block>} in the XML file defines a time-frequency scale of atoms
#' using three parameters:
#' \itemize{
#'   \item \code{windowLen} — length of the analysis window (in samples),
#'   \item \code{windowShift} — step size between consecutive windows,
#'   \item \code{fftSize} — FFT size defining frequency resolution.
#' }
#'
#' Each block generates a grid of atoms over time and frequency bins, forming
#' a multiresolution Gabor dictionary. Smaller windows provide better time
#' resolution, while larger windows improve frequency resolution.  If the signal
#' duration is shorter than the window length, no time positions are generated
#' for that block.
#'
#' @note The function assumes a specific XML structure containing \code{block}
#' and nested \code{param} nodes with attributes \code{name} and \code{value}.
#'
#' @section Usage in sparse decomposition pipeline:
#' The output of \code{read_dict()} is a low-level dictionary of atom
#' parameters (time-frequency grid description). It serves as an input
#' to \code{topk_atoms()}, which:
#' \itemize{
#'   \item evaluates complex Gabor atoms,
#'   \item computes phase-invariant cross-correlations with the signal,
#'   \item selects the best \code{topk} atoms per channel,
#'   \item constructs a full real-valued atom representation with optimal phase.
#' }
#'
#' The resulting \code{"topk"} object contains precomputed atoms and metadata
#' that are directly consumed by \code{omp_core()} for sparse decomposition.
#' In summary, the processing pipeline is:
#' \code{read_dict()} \eqn{\rightarrow} \code{topk_atoms()}
#' \eqn{\rightarrow} \code{omp_core()}.
#'
#' @section Exporting dictionaries from the EMPI program:
#' The EMPI program can export dictionary definitions as an XML file containing
#' atom parameters. This file may include additional elements that are not used
#' in this package, these are safely ignored by the \code{read_dict()} function.
#' This feature enables direct comparison between the EMPI implementation of the
#' Matching Pursuit (MP) algorithm and the Orthogonal Matching Pursuit (OMP)
#' algorithm implemented in \code{omp_core()}.
#' It should be noted that EMPI includes several advanced optimization strategies
#' that are not present in the current OMP implementation. To ensure
#' comparability of results, EMPI is executed with the parameters
#' \code{-o none} and \code{--full-atoms-in-signal}. Their exact meaning is
#' described in the EMPI documentation (see \code{README.md}).
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr
#'
#' @export
#'
#' @seealso
#' \code{\link{topk_atoms}},
#' \code{\link{omp_execute}}
#' \code{\link{omp_core}}
#'
#' @examples
#' # +-------------------------------------------------------------+
#' # | Step 1: Read signal                                         |
#' # +-------------------------------------------------------------+
#' sig_file <- system.file(
#'   "extdata",
#'   "sample3.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' sample3 <- read_csv_signals(
#'   sig_file,
#'   col_names_in_csv = TRUE
#' )
#'
#' sf <- sample3$sampling_frequency
#' duration <- nrow(sample3$signal) / sf
#'
#' # +-------------------------------------------------------------+
#' # | Step 2: Read dictionary definition                          |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample3_dict.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' atoms_dict <- read_dict(
#'   xml_file,
#'   sf,
#'   duration,
#'   verbose = FALSE
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Running the EMPI program with the                   |
#' # | --dictionary-output option, which allows you to save        |
#' # | (in XML format) data about the dictionary used              |
#' # +-------------------------------------------------------------+
#' dest_dir <- tools::R_user_dir("MatchingPursuit", "cache")
#'
#' opts <- paste0(
#'   "-o none --gabor -i 50 --full-atoms-in-signal --dictionary-output ",
#'   dest_dir,
#'   "/sample3_dict_EMPI.xml"
#' )
#'
#' out_sample3 <- empi_execute(
#'   signal = sample3,
#'   empi_options = opts
#' )
#'
#' # +---------------------------------------------------------------+
#' # | Please compare the sample3_dict.xml and sample3_dict_EMPI.xml |
#' # | files and find out which fields in the latter file are not    |
#' # | used in the read_dict() function.                             |
#' # +---------------------------------------------------------------+
#' con <- file(xml_file, open = "r")
#' for (i in 1:20) {
#'   print(readLines(con, n = 1))
#' }
#' close(con)
#'
#' f <- paste0(dest_dir, "/sample3_dict_EMPI.xml")
#' con <- file(f, open = "r")
#' for (i in 1:20) {
#'   print(readLines(con, n = 1))
#' }
#' close(con)
#'
read_dict <- function (xml_file, sf, duration, verbose = TRUE) {

  signal_length <- sf * duration

  # Parse XML
  doc <- read_xml(xml_file)
  blocks <- xml_find_all(doc, ".//block")

  if (verbose) cat("Number of blocks: ", length(blocks))

  # Decode atoms
  all_atoms <- list()

  atom_index <- 1

  for (block_id in seq_along(blocks)) {
    block <- blocks[[block_id]]
    params_nodes <- xml_find_all(block, ".//param")
    params <- list()
    for (p in params_nodes) {
      name <- xml_attr(p, "name")
      value <- xml_attr(p, "value")
      params[[name]] <- value
    }

    window_len <- as.integer(params[["windowLen"]])
    window_shift <- as.integer(params[["windowShift"]])
    fft_size <- as.integer(params[["fftSize"]])
    # window_opt <- as.numeric(params[["windowopt"]])

    end_time <- signal_length - window_len

    if (end_time >= 0) {
      time_positions <- seq(
        from = 0,
        to = end_time,
        by = window_shift
      )
    } else {
      time_positions <- integer(0)  # no time positions
    }

    freq_bins <- 0:(floor(fft_size / 2) - 1)

    if (verbose) {
      cat(
        "\n===================================\n",
        "Block: ", block_id, "\n",
        "windowLen = ", window_len, "\n",
        "windowShift = ", window_shift, "\n",
        # "windowopt = ", window_opt, "\n",
        "fftSize = ", fft_size, "\n",
        sep = ""
      )
    }

    count <- 0

    for (t in time_positions) {
      for (k in freq_bins) {
        freq_hz <- k * sf / fft_size
        atom <- list(
          block = block_id,
          time_sample = t,
          time_sec = t / sf,
          freq_bin = k,
          freq_hz = freq_hz,
          window_len = window_len,
          # window_opt = window_opt,
          fft_size = fft_size
        )
        all_atoms[[atom_index]] <- atom
        atom_index <- atom_index + 1
        count <- count + 1
      }
    }

    if (verbose) cat("Atoms in block: ", count, sep = "")
  }

  if (verbose) {
    cat("\n===================================\n", sep = "")
    cat("Total atoms: ", length(all_atoms), "\n\n", sep = "")
  }

  # convert to matrix
  mat <- matrix(
    unlist(all_atoms, use.names = FALSE),
    ncol = 7,
    byrow = TRUE
  )
  colnames(mat) <- names(all_atoms[[1]])

  return(mat)
}
