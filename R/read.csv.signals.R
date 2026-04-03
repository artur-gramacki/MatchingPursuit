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
#' out <- read.csv(file, header = FALSE)
#' head(out)
#'
#' signal <- read.csv.signals(file, col.names = "signal_1")
#' head(signal$signal)
#' signal$sampling.rate
#'
read.csv.signals <- function(file, col.names = NULL) {
  line <- readLines(file, n = 1)
  items <- strsplit(line, "\\s+")[[1]]

  if (length(items) != 2) {
    stop("The first line in the file must contain 2 numbers separeted by one or more more whitespace characters. characters.")
  }

  sr <- suppressWarnings(as.numeric(items[1]))
  sl <- suppressWarnings(as.numeric(items[2]))

  if (is.na(sr) || is.na(sl))
    stop("The first line in the file must contain 2 numbers, the first is the sampling rate, the second is the signal length in seconds.")

  signal <- read.table(file, skip = 1, header = FALSE)

  if (nrow(signal) != round(sr * sl))
    stop("The signal must be ", round(sr * sl), " elements long. Now it is ", nrow(signal), " elements long.")

  if (!is.null(col.names)) {
    if (length(col.names) != ncol(signal)) {
      stop("`col.names` has wrong length. It must be ", ncol(signal), ".")
    } else {
      colnames(signal) <- col.names
    }
  } else {
    if (is.null(col.names)) {
      cols <- paste0("v", seq_len(ncol(signal)))
      colnames(signal) <- cols
    }
  }

  return(
    list(signal = signal, sampling.rate = sr)
  )
}
