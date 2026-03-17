#' Checks if the csv file has the correct structure
#'
#'
#' @param file File to be checked. The first line of the file must contain two numbers:
#' the sampling rate in Hz (\code{f}) and the signal length in seconds (\code{s}).
#' This checks whether the file actually contains \code{f*s} samples.
#'
#' @param col.names Vector with column names. If not specified, default names will be created.
#'
#' @return Data frame where rows = samples for all channels, columns = channels.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file, col.names = "signal_1")
#' head(signal)
#'
read.csv.signals <- function(file, col.names = NULL) {
  line <- readLines(file, n = 1)
  items <- strsplit(line, "\\s+")[[1]]

  if (length(items) != 2) {
    stop("The first line in the file must contain 2 numbers.")
  }

  n1 <- suppressWarnings(as.numeric(items[1]))
  n2 <- suppressWarnings(as.numeric(items[2]))

  if (is.na(n1) | is.na(n2))
    stop("The first line in the file must contain 2 numbers, the first is the sampling rate, the second is the signal length in seconds.")

  all.lines <- read.csv(file, header = TRUE)
  if (nrow(all.lines) != n1 * n2)
    stop("The signal must be ", n1 * n2, " elements long. Now it is ", nrow(all.lines), " elements long.")

  if (!is.null(col.names)) {
    if (length(col.names) != ncol(all.lines)) {
      stop("`col.names` has wrong length. It must be ", ncol(all.lines), ".")
    } else {
      colnames(all.lines) <- col.names
    }
  } else {
    if (is.null(col.names)) {
      cols <- paste0("v", seq_len(ncol(all.lines)))
      colnames(all.lines) <- cols
    }
  }

  return(all.lines)
}
