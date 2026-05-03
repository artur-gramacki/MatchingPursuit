#' plots T-F map to visualize the empi decomposition
#'
#' The function is in fact a wrapper for \code{empi2tf()} function where \code{out.mode = "plot"}.
#'
#' @param x \code{empi} object created after executing the \code{empi.execute()} function.
#' In this case, the \code{db.file} parameter must be \code{NULL}.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}. It determines the intensity
#' with which the so-called blobs are displayed on the T-F map.
#'
#' @param freq.divide Specifies how many times the displayed frequency in the T-F map
#' should be decreased. At high sampling rates and when we use a low-pass filter with
#' a typical cut-off frequency much lower than the sampling frequency, a large part of
#' the T-F map does not contain any blobs. If the sampling frequency is \code{f},
#' the maximum frequency in the T-F map will be \code{ceiling(f/2/freq.divide)}
#' (f/2 is the Nyquist rule). If \code{NULL}, it is determined based on the atom
#' with the highest frequency \code{fmax} according to \code{freq.divide=(f/2)/fmax}.
#'
#' @param increase.factor Factor of increasing the number of pixels in the f-axis, the most
#' sensible are non-negative integers (e.g. 2, 4, 5, 8).
#'
#' @param shortening.factor.x Usually, for better visualization of atoms, a value of 2 will
#' be appropriate.
#'
#' @param shortening.factor.y Usually, for better visualization of atoms, a value of 2 will
#' be appropriate.
#'
#' @param display.crosses Whether small crosses should be displayed in the canters of atoms.
#'
#' @param display.atom.numbers Whether atom numbers should be displayed in the canters of atoms.
#'
#' @param display.grid Whether to draw grid lines.
#'
#' @param crosses.color Colour of small crosses.
#'
#' @param palette Palette from the list returned by \code{hcl.pals()} function or the string
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
#' empi.out <- empi.execute(signal = signal)
#'
#' # Plot a time-frequency map based on MP atoms.
#' plot(empi.out)
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
      stop("Object not of class 'empi'.")
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
