#' Read a Gabor dictionary from an XML file
#'
#' The function parses an XML file describing a multiscale Gabor dictionary.
#'
#' @param xml_file
#' Path to the XML file containing the dictionary definition.
#'
#' @param full_atoms_in_signal
#' Logical. If \code{TRUE}, only atoms whose complete support lies within
#' the signal are generated. If \code{FALSE}, atom support may extend beyond
#' the signal boundaries.
#'
#' @param sampling_frequency
#' Sampling frequency (in Hz) of the signal associated with the dictionary.
#'
#' @param duration
#' Duration of the signal (in seconds) used to determine the number
#' of valid time positions.
#'
#' @param verbose
#' Logical; if \code{TRUE}, prints progress information about parsed blocks
#' and generated atoms.
#'
#' @return A matrix where each row describes a Gabor atom with the following columns:
#'
#' \item{block}{Block identifier from the XML file.}
#' \item{time_sample}{Start position of the atom support (in samples).}
#' \item{time_sec}{Start position of the atom support (in seconds).}
#' \item{freq_bin}{Frequency bin index.}
#' \item{freq_hz}{Frequency in Hertz.}
#' \item{window_len}{Window length of the atom support, in samples.}
#' \item{fft_size}{FFT size used to define the frequency grid.}
#'
#' @details
#' Each \code{<block>} in the XML file defines a time-frequency scale of atoms
#' using three parameters:
#' \itemize{
#'   \item \code{windowLen} — length of the analysis window (in samples),
#'   \item \code{windowShift} — step size between consecutive windows,
#'   \item \code{fftSize} — FFT size defining frequency resolution.
#' }
#' The function assumes an XML structure containing \code{param} nodes with
#' \code{name} and \code{value} attributes. An example XML file is shown below.
#' For simplicity, the example contains only one block; in practice, dictionary
#' files usually contain multiple blocks.
#'
#' \preformatted{
#'<?xml version="1.0" encoding="ISO-8859-1"?>
#'<dict>
#'  <block>
#'    <param name="windowLen" value="31"/>
#'    <param name="windowShift" value="2"/>
#'    <param name="fftSize" value="64"/>
#'  </block>
#'</dict>
#' }
#'
#' Each block generates a grid of atoms over time and frequency bins, forming
#' a multiresolution Gabor dictionary. Smaller windows provide better time
#' resolution, while larger windows improve frequency resolution.
#'
#' The treatment of atoms near signal boundaries is controlled by
#' \code{full_atoms_in_signal}.
#'
#' If \code{TRUE}, only atoms fully contained within the signal support are
#' generated. In this case, atom start positions satisfy
#' \deqn{0 \leq t \leq N - L,}
#' where \eqn{t} is the atom start position, \eqn{N} is the signal length,
#' and \eqn{L} is the window length. If the signal is shorter than the window
#' length, no time positions are generated for that block.
#'
#' If \code{FALSE}, atom centres are allowed at positions throughout the
#' signal, and the support of an atom may extend beyond the signal boundaries.
#'
#' @section Usage in sparse decomposition workflow:
#' The output of \code{read_gabor_dict()} is a low-level description of the
#' Gabor time-frequency grid. It serves as input to \code{topk_gabor_atoms()},
#' which:
#' \itemize{
#'   \item evaluates complex Gabor atoms,
#'   \item computes phase-invariant projections onto the signal,
#'   \item selects the best \code{topk} atoms for each channel,
#'   \item constructs real-valued atom representations using optimal phases.
#' }
#'
#' The resulting \code{"topk"} object contains channel-specific atom matrices
#' and associated metadata. Individual atom matrices can subsequently be passed
#' to \code{mp_core()} or \code{omp_core()} for sparse decomposition.
#' The higher-level \code{mp_omp_execute()} function performs these preparation
#' steps internally.
#'
#' @section EMPI compatibility:
#' XML dictionary definitions exported by EMPI can be read directly by this
#' function. Additional XML elements not used by \code{read_gabor_dict()} are
#' ignored. The argument \code{full_atoms_in_signal} controls the boundary
#' convention corresponding to the EMPI \code{--full-atoms-in-signal} option.
#'
#' For full details on the EMPI options and their behavior, see the
#' EMPI documentation in \code{README.md}.
#'
#' @importFrom xml2 read_xml xml_find_all xml_attr
#'
#' @export
#'
#' @seealso
#' \code{\link{topk_gabor_atoms}},
#' \code{\link{mp_omp_execute}},
#' \code{\link{omp_core}},
#' \code{\link{mp_core}},
#' \code{\link{generate_xml_dict}}
#'
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
#' sampling_frequency <- sample3$sampling_frequency
#' duration <- nrow(sample3$signal) / sampling_frequency
#'
#' # +-------------------------------------------------------------+
#' # | Step 2: Read dictionary                                     |
#' # +-------------------------------------------------------------+
#' xml_file <- system.file(
#'   "extdata",
#'   "sample3.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' # +-------------------------------------------------------------+
#' # | Step 3: Compare boundary conventions                        |
#' # +-------------------------------------------------------------+
#' # Generate only atoms whose complete support lies within
#' # the signal boundaries.
#' atoms_full <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = sampling_frequency,
#'   duration = duration,
#'   full_atoms_in_signal = TRUE,
#'   verbose = TRUE
#' )
#'
#' # Allow atom support to extend beyond the signal boundaries.
#' # Atom centres still remain within the signal.
#' atoms_overstep <- read_gabor_dict(
#'   xml_file = xml_file,
#'   sampling_frequency = sampling_frequency,
#'   duration = duration,
#'   full_atoms_in_signal = FALSE,
#'   verbose = TRUE
#' )
#'
#' # Allowing boundary overstep increases the number of dictionary atoms.
#' nrow(atoms_full)
#' nrow(atoms_overstep)
#'
#' # With full_atoms_in_signal = TRUE, atom start positions
#' # are always non-negative.
#' range(atoms_full[, "time_sample"])
#'
#' # With full_atoms_in_signal = FALSE, atoms centred near the beginning
#' # of the signal may have negative start positions.
#' range(atoms_overstep[, "time_sample"])
#'
read_gabor_dict <- function (
    xml_file,
    sampling_frequency,
    duration,
    verbose = FALSE,
    full_atoms_in_signal = FALSE) {

  if (!is.logical(full_atoms_in_signal) ||
      length(full_atoms_in_signal) != 1L ||
      is.na(full_atoms_in_signal)) {
    stop("'full_atoms_in_signal' must be TRUE or FALSE.")
  }

  if (!is.numeric(sampling_frequency) ||
      length(sampling_frequency) != 1L ||
      !is.finite(sampling_frequency) ||
      sampling_frequency <= 0) {
    stop("'sampling_frequency' must be a positive number.")
  }

  if (!is.numeric(duration) ||
      length(duration) != 1L ||
      !is.finite(duration) ||
      duration <= 0) {
    stop("'duration' must be a positive number.")
  }

  signal_length <- round(sampling_frequency * duration)

  # Parse XML
  doc <- read_xml(xml_file)
  blocks <- xml_find_all(doc, ".//block")

  if (length(blocks) == 0L) {
    stop("No dictionary blocks found in the XML file.")
  }

  if (verbose) message("Number of blocks: ", length(blocks))

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

    # Only atoms fully contained within the signal
    if (full_atoms_in_signal) {
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
    } else {
      # Atom centres may span the entire signal.
      # Atom support is allowed to extend beyond signal boundaries.
      center_offset <- floor(window_len / 2)
      center_positions <- seq(
        from = 0,
        to = signal_length - 1L,
        by = window_shift
      )
      time_positions <- center_positions - center_offset
    }

    # Because the signal is real-valued, it is sufficient to consider
    # non-negative frequencies from 0 to the Nyquist frequency.
    freq_bins <- 0:floor(fft_size / 2)

    if (verbose) {
      message(
        "===================================\n",
        "Block: ", block_id, "\n",
        "windowLen = ", window_len, "\n",
        "windowShift = ", window_shift, "\n",
        # "windowopt = ", window_opt, "\n",
        "fftSize = ", fft_size,
        sep = ""
      )
    }

    count <- 0

    for (t in time_positions) {
      for (k in freq_bins) {
        freq_hz <- k * sampling_frequency / fft_size
        atom <- list(
          block = block_id,
          time_sample = t,
          time_sec = t / sampling_frequency,
          freq_bin = k,
          freq_hz = freq_hz,
          window_len = window_len,
          fft_size = fft_size
        )
        all_atoms[[atom_index]] <- atom
        atom_index <- atom_index + 1
        count <- count + 1
      }
    }

    if (verbose) message("Atoms in block: ", count, sep = "")
  }

  if (verbose) {
    message("===================================", sep = "")
    message("Total atoms: ", length(all_atoms), sep = "")
    message("read_gabor_dict() executed successfully.")
    message('XML file can be found in: ', xml_file, "\n")
  }

  if (length(all_atoms) == 0L) {
    stop("No atoms could be generated for the specified signal and dictionary.")
  }

  # convert to matrix
  mat <- matrix(
    unlist(all_atoms, use.names = FALSE),
    ncol = 7,
    byrow = TRUE
  )

  colnames(mat) <- names(all_atoms[[1]])

  ##rm(all_atoms)
  ##gc(FALSE)

  return(mat)
}
