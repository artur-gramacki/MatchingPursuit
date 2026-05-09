#' Reads a selected EDF or EDF+ file and returns signal data
#'
#' The function reads a selected EDF or EDF+ file. Optionally, resampling can be performed
#' (upsampling or downsampling).
#'
#' @param file Path to the EDF / EDF+ file to be read.
#'
#' @param resampling If \code{TRUE}, all signals are resampled
#' (either upsampled or downsampled), depending on the original sampling rates of the channels.
#'
#' @param f.new Target sampling frequency used for upsampling or downsampling.
#'
#' @param from Starting time of the signal to be loaded (in seconds).
#'
#' @param to Ending time of the signal to be loaded (in seconds).
#'
#' @param verbose Logical flag indicating whether progress information should be printed.
#'
#' @details If \code{resampling = TRUE}, signals are resampled according to the target frequency
#' specified by \code{f.new}. Since the EDF standard allows different sampling rates per channel,
#' some channels may be upsampled while others are downsampled. The function does not support
#' independent resampling of individual channels.
#'
#' @importFrom edf read.edf
#' @importFrom utils flush.console
#'
#' @return An object of class \code{edf}, a list containing:
#' 1) a data frame with all signals stored in the EDF file,
#' 2) sampling rate after optional resampling,
#' 3) time stamps after optional resampling,
#' 4) signal names.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' out1  <- read.edf.signals(file, resampling = FALSE)
#'
#' lapply(out1, class)
#' out1$sampling.rate
#'
#' out2 <- read.edf.signals(file, resampling = TRUE, f.new = 128, verbose = TRUE)
#'
#' lapply(out2, class)
#' out2$sampling.rate
#'

read.edf.signals <- function(file, resampling = FALSE, f.new = NULL, from = NULL, to = NULL, verbose = FALSE) {

  if(resampling && !is.numeric(f.new)) {
    stop("`f.new` variable must be a numeric value.")
  }

  if (!is.null(from) && !is.null(to)) {
    if (from >= to) stop("`from` varaible must by smaller than `to`")
  }

  edf <- edf::read.edf(filename = file, read.annotations = FALSE, header.only = FALSE)

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

  if (!eq && !resampling) {
    warning(
      "It has been detected that individual channels do not have the same sampling rate. Therefore, it is not possible to save all channels' data in a single data frame. Run the function again by setting 'resampling = TRUE' and specifying a new frequency value 'f.new'.")
    return(edf)
  }

  signal.names <- NA
  for (i in 1:n.sigs) {
    lab <- edf[["header.signal"]][[i]]$label
    freq <- edf[["header.signal"]][[i]]$samplingrate
    sig <- edf[["signal"]][[i]][["data"]]
    t <- edf[["signal"]][[i]][["t"]]
    f <- edf[["header.signal"]][[i]][["samplingrate"]]

    signal.names[i] <- lab

    if (resampling) {
      sig.len <- length(sig) / f
      sig.new <- signal::resample(sig, p = f.new, q = f, d = 5)
      t.new <- seq(0, sig.len - (1 / f.new), by = 1 / f.new)
      if (verbose) {
        message(
          "Ch", i, ": '", lab, "', ",
          "Fs original: ", freq, " Hz, ",
          "Fs new: ", f.new, " Hz, ",
          "samples: ", length(sig.new), ", ",
          "length: ", length(sig) / f, " sec.")

        flush.console()
      }
    } else {
      sig.new <- sig
      t.new <- t
    }

    if (i == 1) {
      #edf.mtx <- matrix(NaN, nrow = length(sig.new), ncol = n.sigs + 1)
      #colnames(edf.mtx) <- rep("", n.sigs + 1)
      edf.mtx <- matrix(NaN, nrow = length(sig.new), ncol = n.sigs)
      colnames(edf.mtx) <- rep("", n.sigs)
    }

    colnames(edf.mtx)[i] <- lab
    edf.mtx[, i] <- sig.new

    # if (i == n.sigs) {
    #   edf.mtx[, i + 1] <- t.new
    #   colnames(edf.mtx)[i + 1] <- "t"
    # }
  } # for (i in 1:n.sigs)

  if (is.null(f.new)) f.new <- f

  if (!is.null(from) && !is.null(to)) {
    edf.mtx <- edf.mtx[seq(from * f + 1, to * f / (f / f.new)), ]
  }

  my.list <- list(
    signal = as.data.frame(edf.mtx),
    sampling.rate = f.new,
    time.stamps = t.new,
    signal.names = signal.names,
    record.name = basename(file)
  )

  class(my.list) <- "edf"
  return(my.list)
}
