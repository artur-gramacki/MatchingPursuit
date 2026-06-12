#' Performs bipolar, reference or average EEG montage
#'
#' An EEG montage refers to the arrangement of EEG electrodes and the way their signals
#' are displayed relative to one another during electroencephalogram interpretation.
#' The same EEG recording may appear very different depending on the montage used. This
#' function implements the three montage methods most commonly used in practice:
#' 1) Bipolar Montage, 2) Referential (Monopolar) Montage, and
#' 3) Average Reference Montage.
#'
#' @param x Object of class \code{edf} (from \code{read_edf_signals()}).
#'
#' @param montage_type A character string specifying the montage type.
#' \itemize{
#'    \item \code{"average"} - each electrode is referenced to the average of all electrodes
#'    \item \code{"reference"} - each active electrode is compared to a single common reference electrode
#'    \item \code{"bipolar"} - each channel compares two adjacent electrodes
#'}
#' @param ref_channel Name of the reference channel for \code{"reference"} montage.
#'
#' @param bipolar_pairs List of electrodes pairs for \code{"bipolar"} montage. See example below.
#'
#' @return An object of class \code{edf}.
#'
#' @details To check the channel names in the analysed EEG recording,
#' use the \code{read_edf_params()} function.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' out <- read_edf_signals(file, resampling = FALSE, from = 0, to = 10)
#'
#' read_edf_params(file)
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
#' signal_bip_mont <- eeg_montage(out, montage_type = c("bipolar"), bipolar_pairs = pairs)
#' signal_ref_mont <- eeg_montage(out, montage_type = c("reference"), ref_channel = "O1")
#' signal_avg_mont <- eeg_montage(out, montage_type = c("average"))
#'
#' head(signal_bip_mont$signal)
#' head(signal_ref_mont$signal)
#' head(signal_avg_mont$signal)
#'
eeg_montage <- function(
    x,
    montage_type = c("average", "reference", "bipolar"),
    ref_channel = NULL,
    bipolar_pairs = NULL) {

  if (!inherits(x, "edf")) {
    stop("'x' must be an object of class 'edf'.")
  }

  montage_type <- match.arg(montage_type)

  eeg_data <- x$signal

  if (!is.data.frame(eeg_data)) {
    stop("eeg.data must be a dataframe: rows = samples, columns = channels.")
  }

  if (is.null(colnames(eeg_data))) {
    stop("The matrix must have column names (channel names).")
  }

  if (montage_type == "average") {
    avg_signal <- rowMeans(eeg_data)
    reref_data <- sweep(eeg_data, 1, avg_signal, "-")
    return(reref_data)
  }

  if (montage_type == "reference") {
    if (is.null(ref_channel)) {
      stop("Enter the name of the reference channel.")
    }
    if (!(ref_channel %in% colnames(eeg_data))) {
      stop("The reference channel does not exist in the data.")
    }

    ref_signal <- eeg_data[, ref_channel]
    reref_data <- sweep(eeg_data, 1, ref_signal, "-")
    return(reref_data)
  }

  if (montage_type == "bipolar") {
    if (is.null(bipolar_pairs)) {
      stop("Provide a list of channel pairs for bipolar montage.")
    }

    result <- matrix(nrow = nrow(eeg_data), ncol = length(bipolar_pairs))

    new_names <- c()

    for (i in seq_along(bipolar_pairs)) {
      ch1 <- bipolar_pairs[[i]][1]
      ch2 <- bipolar_pairs[[i]][2]

      if (!(ch1 %in% colnames(eeg_data)) || !(ch2 %in% colnames(eeg_data))) {
        stop(paste("Incorrect pair: ", ch1, " ", ch2, sep = ""))
      }

      result[, i] <- eeg_data[, ch1] - eeg_data[, ch2]
      new_names[i] <- paste(ch1, "_", ch2, sep = "")
    }

    colnames(result) <- new_names

    #return(as.data.frame(result))

    my_list <- list(
      signal = as.data.frame(result),
       sampling_frequency = x$ sampling_frequency,
      time_stamps = x$time_stamps,
      signal_names = new_names,
      record_name = x$record_name
    )

    class(my_list) <- "edf"
    return(my_list)

  }
}
