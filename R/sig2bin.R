#' Reads input signal(s) from a data frame and returns them in binary format
#'
#' @description
#' Saves the given data (signals) in binary form.
#' Input signal(s) must be a data frame: rows = samples for all channels, columns = channels.
#' The data frame should have column names. Te function is used internally in the \code{empi.execute}
#' function. The binary data are floating-point values in the byte order  of the current machine
#' (no byte-order conversion is performed). For multichannel signals, first come the samples for all
#' channels at \code{t=0}, then for all channels at \code{t=}\eqn{\Delta}\code{t} and so forth. In other words,
#' the signal should be written in column-major order (rows = channels, columns = samples).
#'
#' @param data Data frame with the input signal(s).
#'
#' @param write.to.file If \code{TRUE}, the file \code{signal.bin} will be created and saved in the current directory.
#'
#' @param file.name The name of the file to generate if \code{write.to.file=TRUE}.
#'
#' @return Input signal saved as the \code{raw}. If \code{write.to.file=TRUE}, the file \code{signal.bin}
#' will additionally be created and saved in the current directory.
#'
#' @note The user does not work directly with \code{.bin} files. Binary files are used only in the
#' \code{empi.execute()} function. The external program (\emph{Enhanced Matching Pursuit Implementation},
#' or \emph{empi} for short) executed inside this function requires binary data as input.
#' Moreover, the ability to convert text files to binary form may be useful if someone wants to work
#' with \emph{empi} independently of the R environment.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file)
#' signal.bin <- sig2bin(data = signal, write.to.file = FALSE)
#'
#' class(signal.bin)
#'
#' # First 4 elements of the signal
#' head(signal, 4)
#'
#' # The same first 4 elements of the signal but in binary (float, 4 bytes)
#' head(signal.bin, 16)
#' ## End(Not run)
#'
sig2bin <- function(data, write.to.file = FALSE, file.name = NULL) {

  signal.raw = raw()
  for (m in 1:nrow(data)) {
    data.row <- as.numeric(data[m, ])
    signal.raw <- c(signal.raw, writeBin(data.row, raw(), size = 4))
  }

  if (write.to.file) {
    if (is.null(file.name)) {
      file.copy(signal.raw, "empi.db", overwrite = TRUE)
      cat("\n--> Note: input signals were also saved in the 'signal.bin' file in the current directory <--\n\n")
    } else {
      file.copy(signal.raw, file.name, overwrite = TRUE)
      cat("\n--> Note: input signals were also saved in the '", file.name, "' file in the current directory <--\n\n", sep = "")
    }
  }

  return(signal.raw)
}

