#' Performs bipolar, reference or average EEG montage
#'
#' An EEG montage is the specific way EEG electrodes are arranged and how their signals are displayed
#' relative to each other during interpretation of an electroencephalogram. The same EEG recording can
#' look very different depending on the montage use. The function implements the three most frequently
#' used montage methods in practice, i.e. 1) Bipolar Montage, 2) Referential (Monopolar) Montage
#' and 3) Average Reference Montage.
#'
#' @param eeg.data Must be a data frame: rows = samples, columns = channels.The data frame must
#' have correct column names (channel names).
#'
#' @param montage.type A character string representing montage type.
#' \itemize{
#'    \item \code{"average"} - each electrode is referenced to the average of all electrodes
#'    \item \code{"reference"} - each active electrode is compared to a single common reference electrode
#'    \item \code{"bipolar"} - each channel compares two adjacent electrodes
#'}
#' @param ref.channel Name of the reference channel for \code{"reference"} montage.
#'
#' @param bipolar.pairs List of electrodes pairs. See example below.
#'
#' @return A data frame with final montage (rows = samples, columns = channels).
#'
#' @details To find out what names the individual channels have in the analysed EEG set,
#' it is worth executing the \code{read.edf.params()} function.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' out <- read.edf.signals(file, resampling = FALSE, from = 0, to = 10)
#' signal <- out$signals
#'
#' read.edf.params(file)
#'
#' # The classical double banana montage.
#' pairs <- list(
#'   c("Fp2", "F4"),
#'   c("F4",  "C4"),
#'   c("C4",  "P4"),
#'   c("P4",  "O2"),
#'   c("Fp1", "F3"),
#'   c("F3",  "C3"),
#'   c("C3",  "P3"),
#'   c("P3",  "O1"),
#'   c("Fp2", "F8"),
#'   c("F8",  "T4"),
#'   c("T4",  "T6"),
#'   c("T6",  "O2"),
#'   c("Fp1", "F7"),
#'   c("F7",  "T3"),
#'   c("T3",  "T5"),
#'   c("T5",  "O1"),
#'   c("Fz",  "Cz"),
#'   c("Cz",  "Pz")
#' )
#'
#' signal.bip.mont <- eeg.montage(signal, montage.type = c("bipolar"), bipolar.pairs = pairs)
#' signal.ref.mont <- eeg.montage(signal, montage.type = c("reference"), ref.channel = "O1")
#' signal.avg.mont <- eeg.montage(signal, montage.type = c("average"))
#'
#' head(signal.bip.mont)
#' head(signal.ref.mont)
#' head(signal.avg.mont)
#'
eeg.montage <- function(
    eeg.data,
    montage.type = c("average", "reference", "bipolar"),
    ref.channel = NULL,
    bipolar.pairs = NULL) {

  montage.type <- match.arg(montage.type)

  if (!is.data.frame(eeg.data)) {
    stop("eeg.data must be a dataframe: rows = samples, columns = channels.")
  }

  if (is.null(colnames(eeg.data))) {
    stop("The matrix must have column names (channel names).")
  }

  if (montage.type == "average") {
    avg.signal <- rowMeans(eeg.data)
    reref.data <- sweep(eeg.data, 1, avg.signal, "-")
    return(reref.data)
  }

  if (montage.type == "reference") {
    if (is.null(ref.channel)) {
      stop("Enter the name of the reference channel.")
    }
    if (!(ref.channel %in% colnames(eeg.data))) {
      stop("The reference channel does not exist in the data.")
    }

    ref.signal <- eeg.data[, ref.channel]
    reref.data <- sweep(eeg.data, 1, ref.signal, "-")
    return(reref.data)
  }

  if (montage.type == "bipolar") {
    if (is.null(bipolar.pairs)) {
      stop("Provide a list of channel pairs for bipolar montage.")
    }

    result <- matrix(nrow = nrow(eeg.data), ncol = length(bipolar.pairs))

    new.names <- c()

    for (i in seq_along(bipolar.pairs)) {
      ch1 <- bipolar.pairs[[i]][1]
      ch2 <- bipolar.pairs[[i]][2]

      if (!(ch1 %in% colnames(eeg.data)) || !(ch2 %in% colnames(eeg.data))) {
        stop(paste("Incorrect pair:", ch1, ch2))
      }

      result[, i] <- eeg.data[, ch1] - eeg.data[, ch2]
      new.names[i] <- paste(ch1, "_", ch2, sep = "")
    }

    colnames(result) <- new.names
    return(as.data.frame(result))
  }
}
