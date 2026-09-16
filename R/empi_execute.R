#' Launches the empi program
#'
#' Runs the EMPI program for the given data (signal).
#'
#' @details
#' The EMPI program (source code and binary files for multiple operating systems) can be
#' downloaded from \url{https://github.com/develancer/empi}. Details are presented in the
#' journal paper: Różański, P. T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with
#' Continuous Dictionaries}. ACM Transactions on Mathematical Software, Volume 50, Issue 3,
#' Article No. 17, pp. 1-17, \doi{10.1145/3674832}.
#'
#' @param signal An object of class \code{sig} returned by \code{read_csv_signals()},
#' an object of class \code{edf} returned by \code{read_edf_signals()},
#' or an object of class \code{wfdb} returned by \code{read_wfdb_signals()}.
#'
#' @param empi_options If \code{NULL}, the EMPI program is run with
#' \code{"-o local --gabor -i 50"} parameters. Otherwise, the user may specify any command-line
#' options. See the \code{README.md} file after downloading the EMPI program using the
#' \code{empi_install()} function.
#'
#' @param  write_to_file If \code{TRUE}, a SQLite database file will be created
#' and saved in the \code{path} directory or, if \code{path = NULL}, in the
#' cache directory. This file stores the results of signal decomposition using the
#' EMPI program.
#'
#' @param path Directory in which the SQLite database file will be saved.
#' If \code{NULL}, the file will be saved in the cache directory.
#'
#' @param file_name Name of the file to create if \code{write_to_file = TRUE}.
#'
#' @param ... Additional arguments passed to \code{system()} when executing
#'   EMPI, such as \code{ignore.stdout = TRUE} or \code{ignore.stderr = TRUE}.
#'
#' @return Results of signal decomposition using the MP algorithm. An object of class
#' \code{mp} is returned. If \code{write_to_file = TRUE}, the results are also written
#' to a SQLite file in the \code{path} directory.
#'
#' \item{atoms}{A data frame describing the selected atoms.}
#' \item{signal}{Matrix containing the original signal(s).}
#' \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#' \item{selected_atoms}{List of matrices containing selected atoms for each channel.}
#' \item{time}{Time vector corresponding to signal samples.}
#' \item{sampling_frequency}{Sampling frequency.}
#'
#'
#' @seealso
#' \code{\link{empi_check}},
#' \code{\link{empi_install}},
#' \code{\link{empi_locate}},
#' \code{\link{plot.mp}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read_csv_signals(file)
#'
#' out_empi <- empi_execute(
#'   signal = signal
#' )
#'
#' # Suppress standard output and standard error
#' out_empi <- empi_execute(
#'   signal = signal,
#'   ignore.stdout = TRUE,
#'   ignore.stderr = TRUE
#' )
#'
#' # The default EMPI options have been modified; see the EMPI README.md for details.
#' # The '--full-atoms-in-signal' option restricts the decomposition to atoms
#' # fully contained within the analyzed signal.
#' # The decomposition results are saved to a SQLite database file.
#' out_empi <- empi_execute(
#'   signal = signal,
#'   empi_options = "-o local --full-atoms-in-signal -i 50 --gabor",
#'   write_to_file = TRUE,
#'   path = NULL,
#'   file_name = "my_decomposition.db"
#' )
#'
#' plot(out_empi, freq_divide = 4)
#' }
#'
empi_execute <- function(
    signal,
    empi_options = NULL,
    write_to_file = FALSE,
    path = NULL,
    file_name = NULL,
    ...)
{

  empi_path <- empi_check()

  if(is.null(empi_path)) {
    stop("EMPI is not installed. Run empi_install() before using empi_execute().")
  }

  if (!inherits(signal, "sig") &&
      !inherits(signal, "edf") &&
      !inherits(signal, "wfdb")) {
    stop("'signal' must be an object of class 'sig', 'edf', or 'wfdb'.")
  }

  sig <- signal$signal
  sampling_frequency <- signal$sampling_frequency

  n_channels <- ncol(sig)

  signal_raw <- signal_to_bin(data = sig, write_to_file = FALSE)

  file_bin <- tempfile(fileext = ".bin")
  file_db <- tempfile(fileext = ".db")

  # cleanup if error
  on.exit(file.remove(file_bin, file_db), add = TRUE)

  writeBin(signal_raw, file_bin)

  if (is.null(empi_options)) {
    options <-  "-o local --gabor -i 50"
  } else {
    options <- empi_options
  }

  command <- paste(
    shQuote(empi_path),
    " ",
    shQuote(file_bin),
    " ",
    shQuote(file_db),
    " ",
    "-f ",
    sampling_frequency,
    " -c ",
    n_channels,
    " --channels 1-",
    n_channels,
    " ",
    options,
    sep = "")

  status <- system(command, ...)

  if (status != 0) {
    stop("EMPI execution failed.", call. = FALSE)
  }

  if (write_to_file) {

    if (is.null(path)) {
      dest_dir <- tools::R_user_dir("MatchingPursuit", "cache")
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      dest_dir <- path
      if (!dir.exists(dest_dir)) {
        ok <- dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(path)) {
          stop("Cannot create directory '", dest_dir, "'.")
        }
      }
    }

    if (is.null(file_name)) {
      temp <- file.path(dest_dir, "empi.db")
      file.copy(file_db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition saved to '", temp, "'.")
    } else {
      temp <- file.path(dest_dir, file_name)
      file.copy(file_db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition saved to '", temp, "'.")
    }
  }

  out <- read_empi_db(file_db)

  return(out)
}
