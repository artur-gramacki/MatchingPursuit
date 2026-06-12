#' Plots a time-frequency (T-F) map to visualize EMPI decomposition
#'
#' This function is a wrapper around \code{tf_map()} with \code{out_mode = "plot"}.
#'
#' @param x An object of class \code{mp} created by \code{empi_execute()}.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}. Determines the intensity
#' with which the so-called blobs are displayed on the T-F map.
#'
#' @param freq_divide Specifies how many times the displayed frequency range in the T-F map
#' should be reduced. At high sampling rates, and when a low-pass filter with
#' a cut-off frequency much lower than the sampling frequency is used, a large part of
#' the T-F map may contain no blobs. If the sampling frequency is \code{f},
#' the maximum frequency in the T-F map will be \code{ceiling(f / 2 / freq_divide)}
#' (\code{f / 2} follows the Nyquist rule). If \code{NULL}, it is determined from the atom
#' with the highest frequency \code{fmax} according to \code{freq_divide = (f / 2) / fmax}.
#'
#' @param increase_factor Factor controlling the increase in the number of pixels along the
#' frequency axis. Non-negative integers such as 2, 4, 5, or 8 are typically appropriate.
#'
#' @param shortening_factor_x Usually, a value of 2 provides better visualization of atoms.
#'
#' @param shortening_factor_y Usually, a value of 2 provides better visualization of atoms.
#'
#' @param display_crosses Whether small crosses should be displayed at the centres of atoms.
#'
#' @param display_atom_numbers Whether atom numbers should be displayed at the centres of atoms.
#'
#' @param display_grid Whether grid lines should be drawn.
#'
#' @param color Color of the small crosses and atom numbers
#'
#' @param palette Palette from the list returned by \code{hcl.pals()} or the string
#' \code{"my custom palette"}.
#'
#' @param plot_signals Whether the original and reconstructed signals should also be displayed.
#'
#' @param ... Currently ignored. Required for compatibility with the generic \code{plot()}.
#'
#' @return No return value, called to visualize the empi decomposition.
#'
#' @examples
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read_csv_signals(file, col_names = "ch1")
#'
#' # Execute the MP algorithm.
#' mp_class <- empi_execute(signal = signal)
#'
#' # Plot a time-frequency map based on MP atoms.
#' plot(mp_class)
#'
#' @export
plot.mp <- function(
    x,
    channel = 1,
    mode = "sqrt",
    freq_divide = NULL,
    increase_factor = 8,
    shortening_factor_x = 2,
    shortening_factor_y = 2,
    display_crosses = TRUE,
    display_atom_numbers = FALSE,
    display_grid = FALSE,
    color = "white",
    palette = "my custom palette",
    plot_signals = TRUE,
    ...
) {

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)

  object <- x

  if (!inherits(object, "mp")) {
    object <- try(x$sf, silent = TRUE)

    if (!inherits(object, "mp")) {
      stop("'x' must be an object of class 'mp'.")
    }
  }

  if (is.null(freq_divide)) {
    rows <- which(object$atoms$channel_id == channel)
    ff <- max(object$atoms$frequency[rows])
    freq_divide <- (object$sf / 2) / ff
  }

  out <- tf_map(
    x = object,
    channel = channel,
    mode = mode,
    freq_divide = freq_divide,
    increase_factor = increase_factor,
    display_crosses = display_crosses,
    display_atom_numbers = display_atom_numbers,
    display_grid = display_grid,
    color = color,
    palette = palette,
    plot_signals = plot_signals,
    out_mode = "plot"
  )

  on.exit(par(old.par))
}
