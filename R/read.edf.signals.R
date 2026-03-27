#' Reads a selected EDF or EDF+ file and returns all signals data
#'
#' The function reads a selected EDF or EDF+ file and returns all signals data as a matrix.
#' Also resampling can be done (upsampling or downsampling).
#'
#' @param file The path to the EDF / EDF+ file to be read.
#'
#' @param resampling Logical \code{TRUE} or \code{FALSE}. If \code{TRUE} the frequency of all signals will be
#' upsampling or downsampling, depending on the actual sampling rate of subsequent channel.
#'
#' @param f.new A new frequency.
#'
#' @param from Loading a signal \code{from} (given as a second).
#'
#' @param to Loading a signal \code{to} (given as a second).
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
#' @importFrom utils flush.console
#'
#' @return A list is returned with:
#' 1) data frame with all signals data stored in given edf file,
#' 2) complete result returned by the \code{edf::read.edf()} function,
#' 3) sampling rate of the data after possible resampling (upsampled or downsampled).
#' It should be added here that an additional column is
#' created in the resulting data frame (as the last one) which contains time stamps.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' sigs1  <- read.edf.signals(file, resampling = FALSE)
#'
#' lapply(sigs1, class)
#' sigs1$sampling.rate
#'
#' # Note that the last column (t) changes after resampling.
#' sigs2 <- read.edf.signals(file, resampling = TRUE, f.new = 128)
#'
#' lapply(sigs2, class)
#' sigs2$sampling.rate
#'

read.edf.signals <- function(file, resampling = FALSE, f.new = NULL, from = NULL, to = NULL, verbose = FALSE) {

  if(resampling == TRUE & !is.numeric(f.new)) {
    stop("`f.new` variable must be a numeric value.")
  }

  if (!is.null(from) & !is.null(to)) {
    if (from >= to) stop("`from` varaible must by smaller than `to`")
  }

  edf <- read.edf(filename = file, read.annotations = FALSE, header.only = FALSE)

  if (nchar(edf[["header.global"]][["reserved"]]) > 0) {
    n.sigs <- edf[["header.global"]][["n.signals"]] - 1 # EDF +
  } else {
    n.sigs <- edf[["header.global"]][["n.signals"]]     # EDF
  }

  ff <- NA
  for (i in 1:n.sigs) {
    ff[i] <- edf[["header.signal"]][[i]][["samplingrate"]]
  }
  eq <- length(unique(ff)) == 1

  if (!eq & !resampling) {
    warning(
      "It has been detected that individual channels do not have the same sampling rate. Therefore, it is not possible to save all channels' data in a single data frame. Run the function again by setting 'resampling = TRUE' and specifying a new frequency value 'f.new'.")
    return(edf)
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
        message(
          "Channel '", lab, "', ",
          "original sampling rate: ", freq, " Hz, ",
          "new sampling rate: ", f.new, " Hz, ",
          "number of samples: ", length(sig), ", ",
          "signal length: ", length(sig) / f, " sec.")

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
  } # for (i in 1:n.sigs)

  if (is.null(f.new)) f.new <- f

  if (!is.null(from) & !is.null(to)) {
    edf.mtx <- edf.mtx[seq(from * f + 1, to * f / (f / f.new)), ]
  }

  return(list(
    signals = as.data.frame(edf.mtx),
    edf = edf,
    sampling.rate = f.new)
  )
}
