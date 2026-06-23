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
#' @param signal List containing the signal in a data frame together with its sampling frequency.
#' The data frame should have meaningful column names (channel names).
#' The list must contain elements named \code{"signal"} and \code{"sampling_frequency"}.
#'
#'
#' @param empi_options If \code{NULL}, the EMPI program is run with
#' \code{"-o local --gabor -i 50 --cpu-workers 8"} parameters. Otherwise, the user may specify any command-line
#' options. See the \code{README.md} file after downloading the EMPI program using the
#' \code{empi_install()} function.
#'
#' @param  write_to_file If \code{TRUE}, a SQLite database file will be created
#' and saved in the \code{path} directory or, if \code{path = NULL}, in the
#' cache directory. This file stores the results of signal decomposition using the MP algorithm
#'
#' @param path Directory in which the SQLite database file will be saved.
#' If \code{NULL}, the file will be saved in the cache directory.
#'
#' @param file_name Name of the file to create if \code{write_to_file = TRUE}.
#'
#' @return Results of signal decomposition using the MP algorithm. An object of class
#' \code{mp} is returned. If \code{write_to_file = TRUE}, the results are also written
#' to a SQLite file in the \code{path} directory.
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
#' \dontrun{
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' out <- read_csv_signals(file)
#'
#' out_empi <- empi_execute(
#'   signal = out,
#'   empi_options = NULL,
#'   write_to_file = FALSE,
#'   path = NULL,
#'   file_name = NULL
#' )
#'
#' plot(out_empi)
#' }
#'
empi_execute <- function(
    signal,
    empi_options = NULL,
    write_to_file = FALSE,
    path = NULL,
    file_name = NULL)
{

  empi_path <- empi_check()

  if(is.null(empi_path)) {
    return()
  }

  if (!all(c("signal", "sampling_frequency") %in% names(signal))) {
    stop("Input list must contain 'signal' and 'sampling_frequency'.")
  }

  sig <- signal$signal
  sampling_frequency <- signal$sampling_frequency

  n_channels <- ncol(sig)

  signal_raw <- sig2bin(data = sig, write_to_file = FALSE)

  file_bin <- tempfile(fileext = ".bin")
  file_db <- tempfile(fileext = ".db")

  # cleanup if error
  on.exit(file.remove(file_bin, file_db), add = TRUE)

  writeBin(signal_raw, file_bin)

  if (is.null(empi_options)) {
    options <-  "-o local --gabor -i 50 --cpu-workers 8"
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

  status <- system(command)

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

  out <- read_empi_db_file(file_db)

  return(out)
}
