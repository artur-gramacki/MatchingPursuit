#' Reads a selected EDF or EDF+ file and returns all signals data
#'
#' The function reads a selected EDF or EDF+ file and returns all signals data as a matrix.
#' Also resampling can be done (upsampling or downsampling).
#'
#' @param file The full path to the EDF/EDF+ file to be read.
#'
#' @param resampling Logical TRUE or FALSE. If TRUE the frequency of all signals will be
#' upsampling or downsampling, depending on the actual sampling rate of subsequent channel.
#'
#' @param f.new A new frequency.
#'
#' @param from Loading a signal \code{from} the given second.
#'
#' @param to Loading a signal \code{to} the given second.
#'
#' @param verbose Flag to print out progress information.
#'
#' @details If \code{resampling=TRUE}, the frequency of all signals will be upsampled or downsampled,
#' depending on the actual sampling rate of the individual channels and the set value of the
#' \code{f.new} parameter. The EDF standard assumes that each channel can be sampled at a different
#' rate. Therefore, it may happen that some channels are upsampled and others are downsampled. The
#' function does not provide the functionality to independently change the sampling rate for each channel.
#'
#' @importFrom edf read.edf
#' @importFrom signal resample
#' @importFrom utils flush.console
#'
#' @return A matrix \code{edf.mtx} with all signals data is returned and a list \code{edf} as a
#' result of executing \code{edf::read.edf()} function.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' sigs1  <- read.edf.signals(file, resampling = FALSE)
#' head(sigs1)
#' sigs2 <- read.edf.signals(file, resampling = TRUE, f.new = 128)
#' head(sigs2)
#'

read.edf.signals <- function(file, resampling = TRUE, f.new = NULL, from = NULL, to = NULL, verbose = FALSE) {

  if(resampling == TRUE & !is.numeric(f.new)) {
    stop("\n--> `f.new` variable must be a numeric value. <--")
  }

  if (!is.null(from) & !is.null(to)) {
    if (from >= to) stop("\n--> `from` varaible must by smaller than `to`  ")
  }

  edf <- read.edf(filename = file, read.annotations = FALSE, header.only = FALSE)

  if (nchar(edf[["header.global"]][["reserved"]]) > 0) {
    n.sigs <- edf[["header.global"]][["n.signals"]] - 1 # EDF +
  } else {
    n.sigs <- edf[["header.global"]][["n.signals"]]     # EDF
  }

  for (i in 1:n.sigs) {
    lab <- edf[["header.signal"]][[i]]$label
    freq <- edf[["header.signal"]][[i]]$samplingrate
    sig <- edf[["signal"]][[i]][["data"]]
    t <- edf[["signal"]][[i]][["t"]]
    f <- edf[["header.signal"]][[i]][["samplingrate"]]

    if (resampling) {
      sig.len <- length(sig) / f
      sig.new <- signal::resample(sig, p = f.new, q = f, d = 5)
      t.new <- seq(0, sig.len - (1 / f.new), by = 1 / f.new)
      if (verbose) {
        cat(
          "Channel '", lab, "', ",
          "original sampling rate: ", freq, " Hz, ",
          "new sampling rate: ", f.new, " Hz, ",
          "number of samples: ", length(sig), ", ",
          "signal length: ", length(sig) / f, " sec.",
          "\n",
          sep = "")
        flush.console()
      }
    } else {
      sig.new <- sig
      t.new <- t
    }

    if (i == 1) {
      edf.mtx <- matrix(NaN, nrow = length(sig.new), ncol = n.sigs + 1)
      colnames(edf.mtx) <- rep("", n.sigs + 1)
    }

    colnames(edf.mtx)[i] <- lab
    edf.mtx[, i] <- sig.new

    if (i == n.sigs) {
      edf.mtx[, i + 1] <- t.new
      colnames(edf.mtx)[i + 1] <- "t"
    }
  }

  if (is.null(f.new)) f.new <- f

  if (!is.null(from) & !is.null(to)) {
    edf.mtx <- edf.mtx[seq(from * f + 1, to * f / (f / f.new)), ]
  }

  return(signals = as.data.frame(edf.mtx))
}
