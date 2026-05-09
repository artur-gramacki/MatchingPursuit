#' The function displays ECG signals in a layout corresponding to standard paper ECG printouts
#'
#' A typical ECG paper layout was used, with a small grid of 0.04 s × 0.1 mV and a
#' large grid of 0.20 s × 0.5 mV.
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom graphics lines segments
#' @importFrom stats median
#'
#' @param x Object of class \code{ecg} (from \code{read.ecg.signals()}).
#'
#' @param begin Time point (in seconds) at which to start plotting.
#'
#' @param end Time point (in seconds) at which to stop plotting.
#'
#' @param panel.height Number of large squares to display (according to standard ECG paper):
#' \itemize{
#'     \item small grid: 0.04 sec. x 0.1 mV
#'     \item large grid: 0.20 sec. x 0.5 mV
#' }
#'
#' @param small.squares If \code{TRUE}, the small grid is also displayed.
#'
#' @param zero.line If \code{TRUE}, a horizontal line representing \code{0 mV} is displayed.
#'
#' @param ... Currently ignored. Required for compatibility with the generic \code{plot()}.
#'
#' @return No return value, called to visualize an ECG graph.
#'
#' @export
#'
#' @examples
#' # ECG data comes from https://physionet.org/content/ptb-xl/1.0.3/
#' file <- system.file("extdata", "00001_lr.hea", package = "MatchingPursuit")
#' dir <- dirname(file)
#' name <- tools::file_path_sans_ext(basename(file))
#'
#' out <- read.ecg.signals(file)
#'
#' plot(
#'   x = out,
#'   begin = 0,
#'   end = 10,
#'   panel.height = 2,
#'   zero.line = FALSE,
#'   small.squares = TRUE
#' )
#'
plot.ecg <- function(
    x,
    begin,
    end,
    panel.height = 3,
    small.squares = TRUE,
    zero.line = FALSE,
    ...
) {

  ## Standard ECG paper
  ## Small grid: 0.04 s x 0.1 mV
  ## Large grid: 0.20 s x 0.5 mV

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)

  if (!inherits(x, "ecg")) {
    stop("'x' must be an object of class 'ecg'.")
  }

  ecg <- as.matrix(x$signal)
  fs <- x$sampling.rate
  channels <- ncol(ecg)

  main <- paste("record name: ", x$record.name, sep = "")

  # Each column is centered around its median. In signals like ECG/EGM, this
  # helps remove the base-level offset (DC offset), making channels more comparable.
  # Following this line, each channel has a median of approximately zero.
  md <- apply(ecg, 2, median)
  ecg <- sweep(ecg, 2, md, "-")

  from <- begin * fs
  to <- end * fs

  ecg <- ecg[from:to, ]

  lead.names <- colnames(ecg)
  n <- nrow(ecg)

  # time points
  t <- seq(begin, by = 1 / fs, length.out = n)

  # duration of ECG signal (in sec.)
  duration <- n / fs

  # panel.height - single strip height (mV)
  ph2 <- panel.height / 2

  baseline <- rev(seq(0, by = panel.height, length.out = channels))

  ylim <- c(-ph2, max(baseline) + ph2)

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
    cex.main = 1
  )

  for (i in 1:channels) {

    y0 <- baseline[i]

    if (small.squares) {
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

    ## large horizontal grids: 0.2 sec
    for (y in seq(y0 - ph2, y0 + ph2, by = 0.5)) {
      segments(begin, y, end, y, col = "#e4a0a0", lwd = 1)
    }

    ## baseline
    if (zero.line)  segments(begin, y0, end, y0, col = "blue", lwd = 0.5)

    ## signal
    # range(ecg[, i])
    # head(t); tail(t)
    lines(t, ecg[, i] + y0, lwd = 1)

    ## lead names
    shift <- (end - begin) * 0.02
    text(begin - shift, y0, lead.names[i], xpd = TRUE, adj = 1)
  }

  axis(1,
       at = seq(begin, end, by = 1),
       labels = seq(begin, end, by = 1),
       lwd = 0,
       lwd.ticks = 1)

  on.exit(par(old.par))
}

