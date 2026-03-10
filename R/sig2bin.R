#' Reads input signal(s) from a data frame and returns them in binary format
#'
#' @description
#' Input signal(s) must be a data frame: rows = samples for all channels, columns = channels.
#' The data frame should have column names. Te function is used internally in the \code{empi.execute}
#' function. The binary data are floating-point values in the byte order  of the current machine
#' (no byte-order conversion is performed). For multichannel signals, first come the samples for all
#' channels at \code{t=0}, then for all channels at \code{t=Δt}, and so forth. In other words,
#' the signal should be written in column-major order (rows = channels, columns = samples).
#'
#' @param data Data frame with the input signal(s).
#' @param write.to.file If TRUE, the file \code{signal.bin} will be created and saved in the current directory.
#'
#' @return Input signal saved as the \code{raw}. If \code{write.to.file=TRUE}, the file \code{signal.bin} will conditionally be created and saved in the current directory.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv(file, header = TRUE)
#' signal.bin <- sig2bin(data = signal, write.to.file = FALSE)
#'
#' class(signal.bin)
#'
#' # First 4 elements in signal
#' head(signal, 4)
#'
#' #' # First 4 elements in signal in binary (float, 4 bytes)
#' head(signal.bin, 16)
#' ## End(Not run)
#'
sig2bin <- function(data, write.to.file = TRUE) {

  signal.raw = raw()
  for (m in 1:nrow(data)) {
    data.row <- as.numeric(data[m, ])
    signal.raw <- c(signal.raw, writeBin(data.row, raw(), size = 4))
  }

  if (write.to.file) {
    writeBin(signal.raw, "signal.bin")
    cat("\n--> Note: input signals were also saved in the 'signal.bin' file in the current directory <--\n\n")
  }

  return(signal.raw)
}

