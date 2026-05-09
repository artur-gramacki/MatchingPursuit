#' The function displays EEG signals
#'
#' Signals are displayed one below another and may be shown in different
#' colours for improved readability.
#'
#' @importFrom graphics lines segments
#' @importFrom stats median
#'
#' @param x Object of class \code{edf} (from \code{read.edf.signals()}).
#'
#' @param begin Time point (in seconds) at which to start plotting.
#'
#' @param end Time point (in seconds) at which to stop plotting.
#'
#' @param panel.height Controls the vertical spacing between individual signals.
#' If \code{NULL}, the value is chosen automatically so that all signals are clearly
#' visible and do not overlap.
#'
#' @param rainbow If \code{TRUE}, individual channels are drawn in different colours.
#'
#' @param bg.colour Background colour.
#'
#' @param txt.col Colour of text elements (axis labels and title).
#'
#' @param zero.line If \code{TRUE}, a horizontal line representing \code{0 mV} is displayed.
#'
#' @param main The text shown as the plot title.
#'
#' @param ... Currently ignored. Required for compatibility with the generic \code{plot()}.
#'
#' @return No return value, called to visualize an EEG graph.
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG.edf", package = "MatchingPursuit")
#' out  <- read.edf.signals(file, resampling = FALSE)
#'
#' plot(
#'   x = out,
#'   begin = 0,
#'   end = 10,
#'   panel.height = NULL,
#'   rainbow = TRUE,
#'   bg.colour = "black",
#'   txt.col = "white",
#'   zero.line = TRUE,
#'   main = "EEG signals stored in the EEG.edf file"
#' )
#'
#' plot(
#'   x = out,
#'   begin = 0,
#'   end = 10,
#'   panel.height = NULL,
#'   rainbow = FALSE,
#'   bg.colour = "white",
#'   txt.col = "black",
#'   zero.line = TRUE,
#'   main = "EEG signals stored in the EEG.edf file"
#' )

plot.edf <- function(
    x,
    begin,
    end,
    panel.height = NULL,
    rainbow = TRUE,
    bg.colour = "black",
    txt.col = "white",
    zero.line = TRUE,
    main = NULL,
    ...
) {

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)

  if (!inherits(x, "edf")) {
    stop("'x' must be an object of class 'edf'.")
  }

  par(bg = bg.colour)

  eeg <- as.matrix(x$signal)
  fs <- x$sampling.rate
  channels <- ncol(eeg)
  if(rainbow) {
    line.cols <- rep(rainbow(6), length.out = channels)
  } else {
    line.cols <- rep("black", length.out = channels)
  }

  if (is.null(main)) main <- paste("file name: ", x$record.name, sep = "")

  # Each column is centered around its median. In signals like ECG/EGM, this
  # helps remove the base-level offset (DC offset), making channels more comparable.
  # Following this line, each channel has a median of approximately zero.
  md <- apply(eeg, 2, median)
  eeg <- sweep(eeg, 2, md, "-")

  from <- begin * fs
  to <- end * fs

  eeg <- eeg[from:to, ]

  lead.names <- colnames(eeg)
  n <- nrow(eeg)

  # time points
  t <- seq(begin, by = 1 / fs, length.out = n)

  # duration of ECG signal (in sec.)
  duration <- n / fs

  if(is.null(panel.height)) panel.height = ceiling(max(abs(range(eeg)))) / 2
  # panel.height - half of
  ph2 <- panel.height / 2

  baseline <- rev(seq(0, by = panel.height, length.out = channels))

  b <- baseline[1] - baseline[2]

  ylim <- c(-ph2 - b , max(baseline) + ph2 + b)

  op <- par(mar = c(2, 4, 1, 1), xaxs = "i", yaxs = "i")
  on.exit(par(op))

  plot(
    NA,
    xlim = c(begin, end),
    ylim = ylim,
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = main,
    cex.main = 1,
    col.main = txt.col
  )

  for (i in 1:channels) {

    y0 <- baseline[i]

    ## vertical grids: 1 sec
    for (v in seq(begin, end, by = 1)) {
      segments(v, y0 - ph2, v, y0 + ph2, col = "gray", lwd = 1)
    }

    ## baseline
    if (zero.line)  segments(begin, y0, end, y0, col = "gray", lwd = 0.5)

    ## signal
    # range(eeg[, i])
    # head(t); tail(t)
    lines(t, eeg[, i] + y0, lwd = 1, col = line.cols[i])

    ## lead names
    shift <- (end - begin) * 0.02
    text(begin - shift, y0, lead.names[i], xpd = TRUE, adj = 1, col = line.cols[i])
  }

   axis(1,
       at = seq(begin, end, by = 1),
       labels = seq(begin, end, by = 1),
       lwd = 0,
       lwd.ticks = 1, col = txt.col,
       col.axis = txt.col,
       padj = -1.5,
       tcl = 0.6)

  on.exit(par(old.par))

  message("Actual value of 'panel.height' parameter is: ", panel.height)
}

