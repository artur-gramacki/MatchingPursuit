#' Reads and checks if the csv file has the correct structure
#'
#'
#' @param file File to be read and check. The first line of the file must contain two numbers:
#' the sampling rate in Hz (\code{freq}) and the signal length in seconds (\code{sec}).
#' The function checks whether the file actually contains \code{round(freq*sec)} samples. The two numbers
#' must by separated by one or more whitespace characters.
#'
#' @param col.names Vector with column names. If not specified, default names will be created.
#'
#' @param col.names.in.csv if \code{TRUE}, we assume that the second line contains column names.
#'
#' @importFrom utils read.table
#'
#' @return A list is returned with:
#' 1) data frame where rows = samples for all channels, columns = channels, 2) sampling rate.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#'
#' # The first line of the file must contain two numbers:
#' # a) the sampling rate in Hz
#' # b) the signal length in seconds
#' out <- read.table(file, header = FALSE, skip = 1)
#' head(out)
#'
#' signal <- read.csv.signals(file, col.names = "signal_1")
#' head(signal$signal)
#' signal$sampling.rate
#'
#' file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file, col.names = c("signal_1", "signal_2", "signal_3"))
#' head(signal$signal)
#' signal$sampling.rate
#'
#' # Now, the csv file contains signal names in the second line
#' file <- system.file("extdata", "EEG.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file, col.names.in.csv = TRUE)
#' head(signal$signal)
#' signal$sampling.rate
#'
#' # Now, the csv file contains signal names in the second line
#' # The data here is the same as in the EEG.csv file, but after performing
#' # 'double banana' montages and after applying filtering.
#' file <- system.file("extdata", "EEG_bipolar_filtered.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file, col.names.in.csv = TRUE)
#' head(signal$signal)
#' signal$sampling.rate
#'
read.csv.signals <- function(file, col.names = NULL, col.names.in.csv = FALSE) {
  line <- readLines(file, n = 1)
  items <- strsplit(line, "\\s+")[[1]]

  if (length(items) != 2) {
    stop("The first line in the file must contain 2 numbers separeted by one or more whitespace characters.")
  }

  sr <- suppressWarnings(as.numeric(items[1]))
  sl <- suppressWarnings(as.numeric(items[2]))

  if (sr <= 0 || sl <= 0) stop("Numbers in the first line must be positive.")

  if (is.na(sr) || is.na(sl))
    stop("The first line in the file must contain 2 numbers, the first is the sampling rate, the second is the signal length in seconds.")

  if (col.names.in.csv) {
    signal <- read.table(file, skip = 2, header = FALSE)
  } else {
    signal <- read.table(file, skip = 1, header = FALSE)
  }

  if (nrow(signal) != round(sr * sl))
    stop("The signal must be ", round(sr * sl), " elements long. Now it is ", nrow(signal), " elements long.")

  if (!is.null(col.names)) {
    if (length(col.names) != ncol(signal)) {
      stop("`col.names` has wrong length. It must be ", ncol(signal), ".")
    } else {
      colnames(signal) <- col.names
    }
  }

  if (is.null(col.names) && !col.names.in.csv) {
    cols <- paste0("v", seq_len(ncol(signal)))
    colnames(signal) <- cols
  }

  if (is.null(col.names) && col.names.in.csv) {
    line <- readLines(file, n = 2)
    cols <- strsplit(line, "\\s+")[[2]]
    colnames(signal) <- cols
  }

  return(list(signal = signal, sampling.rate = sr))
}
