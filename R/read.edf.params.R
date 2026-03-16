#' Reads a selected EDF or EDF+ file and returns signal parameters
#'
#' @description
#' The function reads a selected EDF or EDF+ file and returns selected signals parameters
#' (channel names, frequency of each channel, number of samples in each channel
#' and the length of each channel in seconds). Additional information stored in EDF+
#' files (such as interrupted recordings, time-stamped annotations) is not used in the
#' package and is therefore not read.
#'
#' @param file The full path to the EDF/EDF+ file to be read.
#'
#' @return A data frame is returned containing the most basic parameters of the EDF / EDF(+) file.
#'
#' @importFrom edf read.edf
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' read.edf.params(file)
#'
read.edf.params <- function(file) {

  edf <- read.edf(filename = file, read.annotations = FALSE, header.only = FALSE)
  signals <- data.frame()
  n.sigs <- edf[["header.global"]][["n.signals"]]

  for (i in 1:n.sigs) {
    signals[i, 1] <- edf[["header.signal"]][[i]]$label
    signals[i, 2] <- edf[["header.signal"]][[i]]$samplingrate
    signals[i, 3] <- length(edf[["signal"]][[i]][["data"]])
    signals[i, 4] <- signals[i, 3] / signals[i, 2]
  }
  colnames(signals) <- c("channel.name", "frequency", "no.of.samples", "length.sec")
  return(signals)
}
