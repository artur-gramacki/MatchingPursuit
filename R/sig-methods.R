#' Methods for Signal Objects
#'
#' Methods for printing, summarizing, and plotting objects of class
#' \code{"sig"}, together with a print method for objects returned by
#' \code{summary()}.
#'
#' Objects of class \code{"sig"} represent a single- or multi-channel signal
#' together with its sampling frequency and corresponding time vector.
#'
#' @name sig-methods
#'
#' @param x An object of class \code{"sig"} or \code{"summary.sig"}.
#' @param object An object of class \code{"sig"}.
#' @param ... Additional arguments. For \code{plot.sig()}, these are passed
#'   to \code{plot()}; otherwise they are currently ignored.
#'
#' @seealso
#' \code{\link{read_csv_signals}},
#' \code{\link{as_sig}}
#'
#' @return
#' \code{print.sig()} and \code{print.summary.sig()} return their input
#' object invisibly.
#'
#' \code{summary.sig()} returns an object of class \code{"summary.sig"}
#' containing signal dimensions, sampling information, channel names,
#' and basic descriptive statistics for each channel.
#'
#' @examples
#' file <- system.file("extdata", "sample3.csv", package = "MatchingPursuit")
#'
#' x <- read_csv_signals(
#'   file,
#'   col_names_in_csv = TRUE
#' )
#'
#' print(x)
#' summary(x)
#'
#' plot(x)
#' plot(x, mar = c(4, 12, 2, 1))
#' plot(x, mar = c(4, 12, 2, 1), col = "blue")
#' plot(x, col = "red", lwd = 2, lty = 4)
#' plot(x, begin = 0, end = 1, main = "Signal")
#'
NULL

#' @rdname sig-methods
#' @export
print.sig <- function(x, ...) {

  n_samples <- nrow(x$signal)
  n_channels <- ncol(x$signal)
  duration <- n_samples / x$sampling_frequency

  cat("Signal object (class 'sig')\n")
  cat("--------------------------------------------------\n")
  cat("Samples:            ", n_samples, "\n", sep = "")
  cat("Channels:           ", n_channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(duration, 6), " s\n", sep = "")

  invisible(x)
}

#' @rdname sig-methods
#' @export
summary.sig <- function(object, ...) {

  n_samples <- nrow(object$signal)
  n_channels <- ncol(object$signal)
  duration <- n_samples / object$sampling_frequency

  channel_names <- colnames(object$signal)

  if (is.null(channel_names)) {
    channel_names <- paste0("signal_", seq_len(n_channels))
  }

  signal <- as.data.frame(object$signal)

  statistics <- data.frame(
    channel = channel_names,
    min = vapply(signal, min, numeric(1), na.rm = TRUE),
    mean = vapply(signal, mean, numeric(1), na.rm = TRUE),
    sd = vapply(signal, stats::sd, numeric(1), na.rm = TRUE),
    max = vapply(signal, max, numeric(1), na.rm = TRUE),
    missing = vapply(signal, function(z) sum(is.na(z)), integer(1)),
    row.names = NULL
  )

  out <- list(
    samples = n_samples,
    channels = n_channels,
    sampling_frequency = object$sampling_frequency,
    duration = duration,
    channel_names = channel_names,
    statistics = statistics
  )

  class(out) <- "summary.sig"

  out
}

#' @rdname sig-methods
#' @export
print.summary.sig <- function(x, ...) {

  cat("Summary of Signal object (class 'sig')\n")
  cat("--------------------------------------------------\n")
  cat("Samples:            ", x$samples, "\n", sep = "")
  cat("Channels:           ", x$channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(x$duration, 6), " s\n", sep = "")

  cat("\nChannel statistics:\n")
  print(x$statistics, row.names = FALSE)

  invisible(x)
}


#' @rdname sig-methods
#'
#' @param begin Beginning of the displayed interval in seconds.
#' @param end End of the displayed interval in seconds.
#' @param panel_height Vertical distance between signal channels. If \code{NULL},
#'   it is determined automatically from the signal amplitude.
#' @param zero_line Logical. If \code{TRUE}, draw a horizontal reference line
#'   for each channel.
#' @param main Optional plot title.
#' @param mar Numeric vector of length four specifying the plot margins in the
#'   form \code{c(bottom, left, top, right)}.
#' @param col Colour used to draw the signal traces.
#' @param lwd Line width used to draw the signal traces.
#' @param lty Line type used to draw the signal traces.
#'
#' @return
#' \code{plot.sig()} is called for its side effect and returns \code{x} invisibly.
#'
#' @importFrom graphics axis lines mtext par plot segments
#' @importFrom stats median
#' @export
plot.sig <- function(
    x,
    begin = NULL,
    end = NULL,
    panel_height = NULL,
    zero_line = TRUE,
    main = NULL,
    mar = c(4, 7, 2, 1),
    col = "black",
    lwd = 1,
    lty = 1,
    ...
) {

  if (!inherits(x, "sig")) {
    stop("'x' must be an object of class 'sig'.")
  }

  signal <- as.matrix(x$signal)
  sampling_frequency <- x$sampling_frequency

  n_samples <- nrow(signal)
  n_channels <- ncol(signal)

  signal_length <- n_samples / sampling_frequency

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

  if (length(mar) != 4L || !is.numeric(mar) || any(!is.finite(mar)) || any(mar < 0)) {
    stop("'mar' must be a numeric vector of length 4 with non-negative values.")
  }

  from <- floor(begin * sampling_frequency) + 1L
  to <- min(floor(end * sampling_frequency) + 1L, n_samples)

  signal <- signal[from:to, , drop = FALSE]

  channel_names <- colnames(signal)

  if (is.null(channel_names)) {
    channel_names <- paste0("signal_", seq_len(n_channels))
  }

  # Center each channel around its median.
  md <- apply(signal, 2, median, na.rm = TRUE)
  signal <- sweep(signal, 2, md, "-")

  if (is.null(panel_height)) {
    max_amplitude <- max(abs(signal), na.rm = TRUE)

    if (!is.finite(max_amplitude) || max_amplitude == 0) {
      panel_height <- 1
    } else {
      panel_height <- 2.5 * max_amplitude
    }
  }

  if (length(panel_height) != 1L ||
      !is.numeric(panel_height) ||
      !is.finite(panel_height) ||
      panel_height <= 0) {
    stop("'panel_height' must be NULL or a positive finite numeric value.")
  }

  baseline <- rev(seq(0, by = panel_height, length.out = n_channels))
  half_panel <- panel_height / 2
  ylim <- c(-half_panel, max(baseline) + half_panel)

  time <- seq(
    from = (from - 1L) / sampling_frequency,
    by = 1 / sampling_frequency,
    length.out = nrow(signal)
  )

  old_mar <- par("mar")
  on.exit(par(mar = old_mar), add = TRUE)
  par(mar = mar)

  plot(
    NA,
    xlim = c(begin, end),
    ylim = ylim,
    axes = FALSE,
    xlab = "Time (s)",
    ylab = "",
    main = main,
    xaxs = "i",
    yaxs = "i",
    ...
  )

  axis(1, at = pretty(c(begin, end)))

  for (i in seq_len(n_channels)) {

    if (zero_line) {
      segments(
        x0 = begin,
        y0 = baseline[i],
        x1 = end,
        y1 = baseline[i],
        col = "grey80"
      )
    }

    lines(
      time,
      signal[, i] + baseline[i],
      col = col,
      lwd = lwd,
      lty = lty
    )
  }

  axis(
    side = 2,
    at = baseline,
    labels = channel_names,
    las = 1,
    tick = FALSE
  )

  box()

  invisible(x)
}

