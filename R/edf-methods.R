#' Methods for EDF Objects
#'
#' Methods for printing, summarizing, and plotting objects of class
#' \code{"edf"}, together with a print method for objects returned by
#' \code{summary()}.
#'
#' Objects of class \code{"edf"} represent multi-channel signals imported
#' from EDF files, together with sampling information, channel names,
#' a time vector, and the source record name.
#'
#' @name edf-methods
#'
#' @param x An object of class \code{"edf"} or \code{"summary.edf"}.
#' @param object An object of class \code{"edf"}.
#' @param begin Time point (in seconds) at which to start plotting.
#'   If \code{NULL}, plotting starts at the beginning of the signal.
#' @param end Time point (in seconds) at which to stop plotting.
#'   If \code{NULL}, plotting continues to the end of the signal.
#' @param panel_height Controls the vertical spacing between individual signals.
#'   If \code{NULL}, the value is chosen automatically.
#' @param rainbow Logical. If \code{TRUE}, individual channels are drawn
#'   in different colours.
#' @param bg_colour Background colour.
#' @param txt_col Colour of text elements.
#' @param zero_line Logical. If \code{TRUE}, a horizontal line representing
#'   zero is displayed for each channel.
#' @param main Plot title. If \code{NULL}, the EDF record name is used.
#' @param ... Additional arguments. Currently ignored.
#'
#' @return
#' \code{print.edf()} and \code{print.summary.edf()} return their input
#' object invisibly.
#'
#' \code{summary.edf()} returns an object of class \code{"summary.edf"}
#' containing record information, signal dimensions, sampling information,
#' channel names, and basic descriptive statistics for each channel.
#'
#' \code{plot.edf()} is called for its side effect and returns no value.
#'
#' @seealso
#' \code{\link{read_edf_signals}}
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' x <- read_edf_signals(file, resampling = FALSE)
#'
#' print(x)
#' summary(x)
#'
#' plot(
#'   x,
#'   begin = 0,
#'   end = 10,
#'   panel_height = NULL,
#'   rainbow = TRUE,
#'   bg_colour = "black",
#'   txt_col = "white",
#'   zero_line = TRUE,
#'   main = "EEG signals stored in the EEG.edf file"
#' )
#'
#' plot(
#'   x,
#'   begin = 0,
#'   end = 10,
#'   panel_height = NULL,
#'   rainbow = FALSE,
#'   bg_colour = "white",
#'   txt_col = "black",
#'   zero_line = TRUE,
#'   main = "EEG signals stored in the EEG.edf file"
#' )
NULL

#' @rdname edf-methods
#' @importFrom graphics lines segments
#' @importFrom stats median
#' @export
plot.edf <- function(
    x,
    begin = NULL,
    end = NULL,
    panel_height = NULL,
    rainbow = FALSE,
    bg_colour = "white",
    txt_col = "black",
    zero_line = TRUE,
    main = NULL,
    ...
) {

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par), add = FALSE)

  if (!inherits(x, "edf")) {
    stop("'x' must be an object of class 'edf'.")
  }

  par(bg = bg_colour)

  eeg <- as.matrix(x$signal)
  sampling_frequency <- x$sampling_frequency
  signal_length <- length(x$time) / sampling_frequency
  channels <- ncol(eeg)

  if (rainbow) {
    line.cols <- rep(grDevices::rainbow(6), length.out = channels)
  } else {
    line.cols <- rep("black", length.out = channels)
  }

  if (is.null(main)) main <- paste("file name: ", x$record_name, sep = "")

  # Each column is centered around its median. In signals like EEG/EGM, this
  # helps remove the base-level offset (DC offset), making channels more comparable.
  # Following this line, each channel has a median of approximately zero.
  md <- apply(eeg, 2, median, na.rm = TRUE)
  eeg <- sweep(eeg, 2, md, "-")

  if (is.null(begin)) begin <- 0
  if (is.null(end)) end <- signal_length

  if (length(begin) != 1L || !is.numeric(begin) || !is.finite(begin)) {
    stop("'begin' must be NULL or a single finite numeric value.")
  }

  if (length(end) != 1L || !is.numeric(end) || !is.finite(end)) {
    stop("'end' must be NULL or a single finite numeric value.")
  }

  if (begin < 0 || begin > signal_length) {
    stop("'begin' must be between 0 and the signal length.")
  }

  if (end < 0 || end > signal_length) {
    stop("'end' must be between 0 and the signal length.")
  }

  if (end <= begin) {
    stop("'end' must be greater than 'begin'.")
  }

  from <- floor(begin * sampling_frequency) + 1L
  to <- min(floor(end * sampling_frequency) + 1L, nrow(eeg))
  eeg <- eeg[from:to, , drop = FALSE]

  lead.names <- colnames(eeg)
  n <- nrow(eeg)

  # time points
  t <- seq(begin, by = 1 / sampling_frequency, length.out = n)

  if (is.null(panel_height)) {
    panel_height <- ceiling(max(abs(range(eeg)))) / 2
  }

  # panel_height - half of
  ph2 <- panel_height / 2

  baseline <- rev(seq(0, by = panel_height, length.out = channels))
  b <- baseline[1] - baseline[2]
  ylim <- c(-ph2 - b, max(baseline) + ph2 + b)

  par(mar = c(2, 4, 1, 1), xaxs = "i", yaxs = "i")

  plot(
    NA,
    xlim = c(begin, end),
    ylim = ylim,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    cex.main = 1,
    col.main = txt_col
  )

  for (i in 1:channels) {

    y0 <- baseline[i]

    ## vertical grids: 1 sec
    for (v in seq(begin, end, by = 1)) {
      segments(v, y0 - ph2, v, y0 + ph2, col = "gray", lwd = 1)
    }

    ## baseline
    if (zero_line) segments(begin, y0, end, y0, col = "gray", lwd = 0.5)

    ## signal
    lines(t, eeg[, i] + y0, lwd = 1, col = line.cols[i])

    ## lead names
    shift <- (end - begin) * 0.02
    text(begin - shift, y0, lead.names[i], xpd = TRUE, adj = 1, col = line.cols[i])
  }

  axis(
    1,
    at = seq(begin, end, by = 1),
    labels = seq(begin, end, by = 1),
    lwd = 0,
    lwd.ticks = 1,
    col = txt_col,
    col.axis = txt_col,
    padj = -1.5,
    tcl = 0.6
  )

  message("Actual value of 'panel_height' parameter is: ", panel_height)
  invisible(NULL)
}

#' @rdname edf-methods
#' @export
print.edf <- function(x, ...) {

  n_samples <- nrow(x$signal)
  n_channels <- ncol(x$signal)
  duration <- n_samples / x$sampling_frequency

  cat("EDF signal object (class 'edf')\n")
  cat("--------------------------------------------------\n")
  cat("Record:             ", x$record_name, "\n", sep = "")
  cat("Samples:            ", n_samples, "\n", sep = "")
  cat("Channels:           ", n_channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(duration, 6), " s\n", sep = "")

  invisible(x)
}


#' @rdname edf-methods
#' @export
summary.edf <- function(object, ...) {

  n_samples <- nrow(object$signal)
  n_channels <- ncol(object$signal)
  duration <- n_samples / object$sampling_frequency

  channel_names <- object$signal_names

  statistics <- data.frame(
    channel = channel_names,
    min = vapply(object$signal, min, numeric(1), na.rm = TRUE),
    mean = vapply(object$signal, mean, numeric(1), na.rm = TRUE),
    sd = vapply(object$signal, stats::sd, numeric(1), na.rm = TRUE),
    max = vapply(object$signal, max, numeric(1), na.rm = TRUE),
    missing = vapply(
      object$signal,
      function(z) sum(is.na(z)),
      integer(1)
    ),
    row.names = NULL
  )

  out <- list(
    record_name = object$record_name,
    samples = n_samples,
    channels = n_channels,
    sampling_frequency = object$sampling_frequency,
    duration = duration,
    channel_names = channel_names,
    statistics = statistics
  )

  class(out) <- "summary.edf"

  out
}


#' @rdname edf-methods
#' @export
print.summary.edf <- function(x, ...) {

  cat("Summary of EDF signal object (class 'edf')\n")
  cat("--------------------------------------------------\n")
  cat("Record:             ", x$record_name, "\n", sep = "")
  cat("Samples:            ", x$samples, "\n", sep = "")
  cat("Channels:           ", x$channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(x$duration, 6), " s\n", sep = "")

  cat("\nChannel statistics:\n")
  print(x$statistics, row.names = FALSE)

  invisible(x)
}
