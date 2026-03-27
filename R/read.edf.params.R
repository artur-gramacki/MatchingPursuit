#' Reads a selected EDF or EDF+ file and returns signal parameters
#'
#' @description
#' Reads a selected EDF or EDF+ file and returns selected signals parameters
#' (channel names, frequency of each channel, number of samples in each channel
#' and the length of each channel in seconds). Additional information stored in EDF+
#' files (such as interrupted recordings, time-stamped annotations) is not used in the
#' package and is therefore not read.
#'
#' @param file The path to the EDF/EDF+ file to be read.
#'
#' @return A data frame is returned containing the most basic parameters of the EDF / EDF+ file.
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
  params <- data.frame()
  n.sigs <- edf[["header.global"]][["n.signals"]]

  for (i in 1:n.sigs) {
    params[i, 1] <- edf[["header.signal"]][[i]]$label
    params[i, 2] <- edf[["header.signal"]][[i]]$samplingrate
    params[i, 3] <- length(edf[["signal"]][[i]][["data"]])
    params[i, 4] <- params[i, 3] / params[i, 2]
  }
  colnames(params) <- c("channel.name", "frequency", "no.of.samples", "length.sec")
  return(params)
}
