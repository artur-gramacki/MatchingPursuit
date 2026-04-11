#' Reads input signal(s) from a data frame and returns them in binary format
#'
#' @description
#' Saves the given data (signals) in binary form.
#' Input signal(s) must be a data frame: rows = samples for all channels, columns = channels.
#' The function is used internally in the \code{empi.execute()} function. The binary data are
#' floating-point values in the byte order  of the current machine (no byte-order conversion is performed).
#' For multichannel signals, first come the samples for all channels at \code{t=0}, then for all
#' channels at \code{t=}\eqn{\Delta}\code{t} and so forth. In other words,
#' the signal should be written in column-major order (rows = channels, columns = samples).
#'
#' @param data Data frame with the input signal(s).
#'
#' @param write.to.file If \code{TRUE} the bin file will be created and saved in the cache directory.
#'
#' @return Input signal returned as the \code{raw}. If \code{write.to.file=TRUE}, the \code{.bin} file
#' will additionally be created and saved in the current directory.
#'
#' @note The user does not work directly with \code{.bin} files. Binary files are used only in the
#' \code{empi.execute()} function. The external program (\emph{Enhanced Matching Pursuit Implementation},
#' or EMPI for short) executed inside this function requires binary data as input.
#' Moreover, the ability to convert text files to binary form may be useful if someone wants to work
#' with EMPI independently of the R environment.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#' out <- read.csv.signals(file)
#'
#' signal.bin <- sig2bin(data = out$signal, write.to.file = FALSE)
#'
#' # We have 3 channels. The first 4 time points.
#' head(out$signal, 4)
#'
#' # The same elements of the signal in binary (floats are stored in 4 bytes).
#' head(signal.bin, 48)
#'
#' # After decoding to numeric.
#' # Of course we get the same values as in out$signal.
#' readBin(signal.bin[1:4], what = "numeric", size = 4, endian = "little")
#' readBin(signal.bin[5:8], what = "numeric", size = 4, endian = "little")
#' readBin(signal.bin[41:44], what = "numeric", size = 4, endian = "little")
#' readBin(signal.bin[45:48], what = "numeric", size = 4, endian = "little")
#'
sig2bin <- function(data, write.to.file = FALSE) {

  if (!is.data.frame(data)) {
    stop("data must be a data.frame.")
  }

  signal.raw = raw()
  for (m in 1:nrow(data)) {
    data.row <- as.numeric(data[m, ])
    signal.raw <- c(signal.raw, writeBin(data.row, raw(), size = 4, endian = "little"))
  }

  if (write.to.file) {
    dest.dir <- tools::R_user_dir("MatchingPursuit", "cache")
    temp <- file.path(dest.dir, "signal.bin")
    writeBin(signal.raw, temp)
    message("Binary file saved to '", temp, "'.")
  }

  return(signal.raw)
}

