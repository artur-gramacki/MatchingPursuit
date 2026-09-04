#' Methods for WFDB Objects
#'
#' Methods for printing, summarizing, and plotting objects of class
#' \code{"wfdb"}, together with a print method for objects returned by
#' \code{summary()}.
#'
#' Objects of class \code{"wfdb"} represent multi-channel physiological
#' signals imported from WFDB records, together with their sampling frequency,
#' time vector, lead names, and record name.
#'
#' WFDB (WaveForm DataBase) is a widely used format and software framework
#' for storing, reading, and analyzing physiological time-series signals. It is
#' widely used for signals such as ECG, EEG, blood pressure, respiration, and
#' other biomedical waveforms. It is the file format used by the PhysioNet project
#' and is commonly used in research datasets.
#'
#' A WFDB record typically consists of two main files:
#' \code{.dat}, containing binary signal samples, and \code{.hea}, a header
#' file describing how to interpret the data. Additional annotation files,
#' such as \code{.atr}, may also be present and may contain beat labels or
#' rhythm annotations.
#'
#' The plotting method is designed primarily for ECG signals and displays
#' individual leads in a layout resembling standard ECG paper. The small grid
#' corresponds to 0.04 s by 0.1 mV and the large grid to 0.20 s by 0.5 mV.
#'
#' @name wfdb-methods
#'
#' @param x An object of class \code{"wfdb"} or \code{"summary.wfdb"}.
#'
#' @param object An object of class \code{"wfdb"}.
#'
#' @param begin Time point (in seconds) at which to start plotting.
#'   If \code{NULL}, plotting starts at the beginning of the signal.
#'
#' @param end Time point (in seconds) at which to stop plotting.
#'   If \code{NULL}, plotting continues to the end of the signal.
#'
#' @param panel_height Height of each ECG lead panel (in mV).
#'
#' @param small_squares Logical. If \code{TRUE}, the small ECG-paper grid
#'   (0.04 s by 0.1 mV) is displayed in addition to the large grid.
#'
#' @param zero_line Logical. If \code{TRUE}, a horizontal line representing
#'   0 mV is displayed for each lead.
#'
#' @param ... Additional arguments. Currently ignored.
#'
#' @return
#' \code{print.wfdb()} and \code{print.summary.wfdb()} return their input
#' object invisibly.
#'
#' \code{summary.wfdb()} returns an object of class \code{"summary.wfdb"}
#' containing record information, signal dimensions, sampling information,
#' lead names, and basic descriptive statistics for each lead.
#'
#' \code{plot.wfdb()} is called for its side effect and returns no value.
#'
#' @seealso
#' \code{\link{read_wfdb_signals}}
#'
#' @examples
#' # ECG data from the PTB-XL database
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#' x <- read_wfdb_signals(file)
#'
#' print(x)
#' summary(x)
#'
#' plot(
#'   x,
#'   begin = 0,
#'   end = 10,
#'   panel_height = 1,
#'   zero_line = FALSE,
#'   small_squares = TRUE
#' )
NULL

#' @rdname wfdb-methods
#' @importFrom graphics lines segments
#' @importFrom stats median
#' @export
plot.wfdb <- function(
    x,
    begin = NULL,
    end = NULL,
    panel_height = 3,
    small_squares = TRUE,
    zero_line = FALSE,
    ...
) {

  ## Standard ECG paper
  ## Small grid: 0.04 s x 0.1 mV
  ## Large grid: 0.20 s x 0.5 mV

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par), add = FALSE)

  if (!inherits(x, "wfdb")) {
    stop("'x' must be an object of class 'wfdb'.")
  }

  wfdb <- as.matrix(x$signal)
  sampling_frequency <- x$sampling_frequency
  signal_length <- nrow(wfdb) / sampling_frequency
  channels <- ncol(wfdb)

  main <- paste("record name: ", x$record_name, sep = "")

  # Each column is centered around its median. In signals like ECG/EGM, this
  # helps remove the base-level offset (DC offset), making channels more comparable.
  # Following this line, each channel has a median of approximately zero.
  md <- apply(wfdb, 2, median)
  wfdb <- sweep(wfdb, 2, md, "-")

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

  #from <- begin * sampling_frequency
  from <- begin * sampling_frequency + 1L
  #to <- end * sampling_frequency
  to <- min(floor(end * sampling_frequency) + 1L, nrow(wfdb))
  #wfdb <- wfdb[from:to, ]
  wfdb <- wfdb[from:to, , drop = FALSE]

  lead.names <- colnames(wfdb)
  n <- nrow(wfdb)

  # time points
  t <- seq(begin, by = 1 / sampling_frequency, length.out = n)

  # panel_height - single strip height (mV)
  ph2 <- panel_height / 2

  baseline <- rev(seq(0, by = panel_height, length.out = channels))

  ylim <- c(-ph2, max(baseline) + ph2)

  par(mar = c(2, 4, 1, 1), xaxs = "i", yaxs = "i")

  plot(
    NA,
    xlim = c(begin, end),
    ylim = ylim,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    cex.main = 1
  )

  for (i in 1:channels) {

    y0 <- baseline[i]

    if (small_squares) {
      ## small vertical grids: 0.04 s
      for (x in seq(begin, end, by = 0.04)) {
        segments(x, y0 - ph2, x, y0 + ph2, col = "#f7d7d7", lwd = 0.5)
      }

      ## small horizontal grids: 0.1 mV
      for (y in seq(y0 - ph2, y0 + ph2, by = 0.1)) {
        segments(begin, y, end, y, col = "#f7d7d7", lwd = 0.5)
      }
    }

    ## large vertical grids: 0.2 sec
    for (x in seq(begin, end, by = 0.2)) {
      segments(x, y0 - ph2, x, y0 + ph2, col = "#e4a0a0", lwd = 1)
    }

    ## large horizontal grids: 0.5 mV
    for (y in seq(y0 - ph2, y0 + ph2, by = 0.5)) {
      segments(begin, y, end, y, col = "#e4a0a0", lwd = 1)
    }

    ## baseline
    if (zero_line) segments(begin, y0, end, y0, col = "blue", lwd = 0.5)

    ## signal
    lines(t, wfdb[, i] + y0, lwd = 1)

    ## lead names
    shift <- (end - begin) * 0.02
    text(begin - shift, y0, lead.names[i], xpd = TRUE, adj = 1)
  }

  axis(1,
       at = seq(begin, end, by = 1),
       labels = seq(begin, end, by = 1),
       lwd = 0,
       lwd.ticks = 1)

  invisible(NULL)
}

#' @rdname wfdb-methods
#' @export
print.wfdb <- function(x, ...) {

  n_samples <- nrow(x$signal)
  n_leads <- ncol(x$signal)
  duration <- n_samples / x$sampling_frequency

  cat("WFDB signal object\n")
  cat("------------------------------\n")
  cat("Record:             ", x$record_name, "\n", sep = "")
  cat("Samples:            ", n_samples, "\n", sep = "")
  cat("Leads:              ", n_leads, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(duration, 6), " s\n", sep = "")

  invisible(x)
}

#' @rdname wfdb-methods
#' @export
summary.wfdb <- function(object, ...) {

  n_samples <- nrow(object$signal)
  n_leads <- ncol(object$signal)
  duration <- n_samples / object$sampling_frequency

  lead_names <- object$lead_names

  if (is.null(lead_names)) {
    lead_names <- colnames(object$signal)
  }

  if (is.null(lead_names)) {
    lead_names <- paste0("lead_", seq_len(n_leads))
  }

  signal_df <- as.data.frame(object$signal)

  statistics <- data.frame(
    lead = lead_names,
    min = vapply(signal_df, min, numeric(1), na.rm = TRUE),
    mean = vapply(signal_df, mean, numeric(1), na.rm = TRUE),
    sd = vapply(signal_df, stats::sd, numeric(1), na.rm = TRUE),
    max = vapply(signal_df, max, numeric(1), na.rm = TRUE),
    missing = vapply(
      signal_df,
      function(z) sum(is.na(z)),
      integer(1)
    ),
    row.names = NULL
  )

  out <- list(
    record_name = object$record_name,
    samples = n_samples,
    leads = n_leads,
    sampling_frequency = object$sampling_frequency,
    duration = duration,
    lead_names = lead_names,
    statistics = statistics
  )

  class(out) <- "summary.wfdb"

  out
}

#' @rdname wfdb-methods
#' @export
print.summary.wfdb <- function(x, ...) {

  cat("Summary of WFDB signal object\n")
  cat("------------------------------\n")
  cat("Record:             ", x$record_name, "\n", sep = "")
  cat("Samples:            ", x$samples, "\n", sep = "")
  cat("Leads:              ", x$leads, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(x$duration, 6), " s\n", sep = "")

  cat("\nLead statistics:\n")
  print(x$statistics, row.names = FALSE)

  invisible(x)
}
