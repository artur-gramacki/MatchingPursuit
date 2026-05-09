#' Plots a time-frequency (T-F) map to visualize EMPI decomposition
#'
#' This function is a wrapper around \code{empi2tf()} with \code{out.mode = "plot"}.
#'
#' @param x An object of class \code{empi} created by \code{empi.execute()}.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}. Determines the intensity
#' with which the so-called blobs are displayed on the T-F map.
#'
#' @param freq.divide Specifies how many times the displayed frequency range in the T-F map
#' should be reduced. At high sampling rates, and when a low-pass filter with
#' a cut-off frequency much lower than the sampling frequency is used, a large part of
#' the T-F map may contain no blobs. If the sampling frequency is \code{f},
#' the maximum frequency in the T-F map will be \code{ceiling(f / 2 / freq.divide)}
#' (\code{f / 2} follows the Nyquist rule). If \code{NULL}, it is determined from the atom
#' with the highest frequency \code{fmax} according to \code{freq.divide = (f / 2) / fmax}.
#'
#' @param increase.factor Factor controlling the increase in the number of pixels along the
#' frequency axis. Non-negative integers such as 2, 4, 5, or 8 are typically appropriate.
#'
#' @param shortening.factor.x Usually, a value of 2 provides better visualization of atoms.
#'
#' @param shortening.factor.y Usually, a value of 2 provides better visualization of atoms.
#'
#' @param display.crosses Whether small crosses should be displayed at the centres of atoms.
#'
#' @param display.atom.numbers Whether atom numbers should be displayed at the centres of atoms.
#'
#' @param display.grid Whether grid lines should be drawn.
#'
#' @param crosses.color Colour of the small crosses.
#'
#' @param palette Palette from the list returned by \code{hcl.pals()} or the string
#' \code{"my custom palette"}.
#'
#' @param plot.signals Whether the original and reconstructed signals should also be displayed.
#'
#' @param ... Currently ignored. Required for compatibility with the generic \code{plot()}.
#'
#' @return No return value, called to visualize the empi decomposition.
#'
#' @examples
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file, col.names = "ch1")
#'
#' # Execute the MP algorithm.
#' empi.class <- empi.execute(signal = signal)
#'
#' # Plot a time-frequency map based on MP atoms.
#' plot(empi.class)
#'
#' @export
plot.empi <- function(
    x,
    channel = 1,
    mode = "sqrt",
    freq.divide = NULL,
    increase.factor = 8,
    shortening.factor.x = 2,
    shortening.factor.y = 2,
    display.crosses = TRUE,
    display.atom.numbers = FALSE,
    display.grid = FALSE,
    crosses.color = "white",
    palette = 'my custom palette',
    plot.signals = TRUE,
    ...
) {

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)

  object <- x

  if (!inherits(object, "empi")) {
    object <- try(x$f, silent = TRUE)

    if (!inherits(object, "empi")) {
      stop("'x' must be an object of class 'empi'.")
    }
  }

  if (is.null(freq.divide)) {
    rows <- which(object$atoms$channel_id == channel)
    ff <- max(object$atoms$frequency[rows])
    freq.divide <- (object$f / 2) / ff
    # cat("max atom frequency: ", ff, "\n", sep = "")
    # cat("freq.divide: ", freq.divide, "\n", sep = "")
  }

  out <- empi2tf(
    x = object,
    channel = channel,
    mode = mode,
    freq.divide = freq.divide,
    increase.factor= increase.factor,
    display.crosses = display.crosses,
    display.atom.numbers = display.atom.numbers,
    plot.signals = plot.signals,
    out.mode = "plot"
  )

  on.exit(par(old.par))

}
