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
#' @param sf_new Target sampling frequency used for upsampling or downsampling.
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
#' out1  <- read_edf_signals(file, resampling = FALSE)
#'
#' lapply(out1, class)
#' out1$ sampling_frequency
#'
#' out2 <- read_edf_signals(file, resampling = TRUE, sf_new = 128, verbose = TRUE)
#'
#' lapply(out2, class)
#' out2$ sampling_frequency
#'

read_edf_signals <- function(file, resampling = FALSE, sf_new = NULL, from = NULL, to = NULL, verbose = FALSE) {

  if(resampling && !is.numeric(sf_new)) {
    stop("`sf_new` variable must be a numeric value.")
  }

  if (!is.null(from) && !is.null(to)) {
    if (from >= to) stop("`from` varaible must by smaller than `to`")
  }

  edf <- edf::read.edf(filename = file, read.annotations = FALSE, header.only = FALSE)

  if (nchar(edf[["header.global"]][["reserved"]]) > 0) {
    n_sigs <- edf[["header.global"]][["n.signals"]] - 1 # EDF +
  } else {
    n_sigs <- edf[["header.global"]][["n.signals"]]     # EDF
  }

  ff <- NA
  for (i in 1:n_sigs) {
    ff[i] <- edf[["header.signal"]][[i]][["samplingrate"]]
  }
  eq <- length(unique(ff)) == 1

  if (!eq && !resampling) {
    warning(
      "It has been detected that individual channels do not have the same sampling rate. Therefore, it is not possible to save all channels' data in a single data frame. Run the function again by setting 'resampling = TRUE' and specifying a new frequency value 'sf_new'.")
    return(edf)
  }

  signal_names <- NA
  for (i in 1:n_sigs) {
    lab <- edf[["header.signal"]][[i]]$label
    freq <- edf[["header.signal"]][[i]]$samplingrate
    sig <- edf[["signal"]][[i]][["data"]]
    t <- edf[["signal"]][[i]][["t"]]
    sf <- edf[["header.signal"]][[i]][["samplingrate"]]

    signal_names[i] <- lab

    if (resampling) {
      sig_len <- length(sig) / sf
      sig_new <- signal::resample(sig, p = sf_new, q = sf, d = 5)
      t_new <- seq(0, sig_len - (1 / sf_new), by = 1 / sf_new)
      if (verbose) {
        message(
          "ch", i, ": '", lab, "', ",
          "sf original: ", freq, " Hz, ",
          "sf new: ", sf_new, " Hz, ",
          "samples: ", length(sig_new), ", ",
          "length: ", length(sig) / sf, " sec.")

        flush.console()
      }
    } else {
      sig_new <- sig
      t_new <- t
    }

    if (i == 1) {
      #edf.mtx <- matrix(NaN, nrow = length(sig_new), ncol = n_sigs + 1)
      #colnames(edf_mtx) <- rep("", n_sigs + 1)
      edf_mtx <- matrix(NaN, nrow = length(sig_new), ncol = n_sigs)
      colnames(edf_mtx) <- rep("", n_sigs)
    }

    colnames(edf_mtx)[i] <- lab
    edf_mtx[, i] <- sig_new

    # if (i == n_sigs) {
    #   edf_mtx[, i + 1] <- t_new
    #   colnames(edf_mtx)[i + 1] <- "t"
    # }
  } # for (i in 1:n_sigs)

  if (is.null(sf_new)) sf_new <- sf

  if (!is.null(from) && !is.null(to)) {
    edf_mtx <- edf_mtx[seq(from * sf + 1, to * sf / (sf / sf_new)), ]
  }

  my_list <- list(
    signal = as.data.frame(edf_mtx),
     sampling_frequency = sf_new,
    time_stamps = t_new,
    signal_names = signal_names,
    record_name = basename(file)
  )

  class(my_list) <- "edf"
  return(my_list)
}
