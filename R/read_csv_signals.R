#' Reads and validates a CSV file structure
#'
#'
#' @param file File to be read and checked. The first line of the file must contain two numbers:
#' the sampling frequency in Hz (\code{freq}) and the signal length in seconds (\code{sec}).
#' The function verifies whether the file contains exactly \code{round(freq * sec)} samples.
#' The two numbers must be separated by one or more whitespace characters.
#'
#' @param col_names Optional character vector of column names. If not specified, default names are created.
#'
#' @param col_names_in_csv Logical value. If \code{TRUE}, the second line of the file is assumed
#' to contain column names.
#'
#' @importFrom utils read.table
#'
#' @return A list containing:
#' \itemize{
#'   \item a data frame (rows = samples, columns = channels),
#'   \item sampling frequency.
#' }
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#'
#' # The first line of the file must contain two numbers:
#' # a) the sampling frequency in Hz
#' # b) the signal length in seconds
#' out <- read.csv(file, header = FALSE)
#' head(out)
#'
#' signal <- read_csv_signals(file, col_names = "signal_1")
#' head(signal$signal)
#' signal$ sampling_frequency
#'
#' file <- system.file("extdata", "sample2.csv", package = "MatchingPursuit")
#' signal <- read_csv_signals(file, col_names = c("signal_1"))
#' head(signal$signal)
#' signal$sampling_frequency
#'
#' # Now, the csv file contains signal names in the second line
#' file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#' signal <- read_csv_signals(file, col_names_in_csv = TRUE)
#' head(signal$signal)
#' signal$ sampling_frequency
#'
read_csv_signals <- function(file, col_names = NULL, col_names_in_csv = FALSE) {

  line <- readLines(file, n = 1)
  items <- strsplit(line, "\\s+")[[1]]

  if (length(items) != 2) {
    stop("The first line in the file must contain 2 numbers separated by one or more whitespace characters.")
  }

  sf <- suppressWarnings(as.numeric(items[1]))
  sl <- suppressWarnings(as.numeric(items[2]))

  if (sf <= 0 || sl <= 0) stop("Numbers in the first line must be positive.")

  if (is.na(sf) || is.na(sl)) {
    stop("The first line in the file must contain 2 numbers, the first is the sampling rate, the second is the signal length in seconds.")
  }

  if (col_names_in_csv) {
    signal <- read.table(file, skip = 2, header = FALSE)
  } else {
    signal <- read.table(file, skip = 1, header = FALSE)
  }

  if (nrow(signal) != round(sf * sl)) {
    stop("The signal must be ", round(sf * sl), " elements long. Now it is ", nrow(signal), " elements long.")
  }

  if (!is.null(col_names)) {
    if (length(col_names) != ncol(signal)) {
      stop("`col_names` has wrong length. It must be ", ncol(signal), ".")
    } else {
      colnames(signal) <- col_names
    }
  }

  if (is.null(col_names) && !col_names_in_csv) {
    cols <- paste0("v", seq_len(ncol(signal)))
    colnames(signal) <- cols
  }

  if (is.null(col_names) && col_names_in_csv) {
    line <- readLines(file, n = 2)
    cols <- strsplit(line, "\\s+")[[2]]
    colnames(signal) <- cols
  }

  return(list(signal = signal,  sampling_frequency = sf))
}
