#' Methods for Matching Pursuit Objects
#'
#' Methods for printing, summarizing, and plotting objects of class
#' \code{"mp"}, together with a print method for objects returned by
#' \code{summary()}.
#'
#' Objects of class \code{"mp"} represent the result of a Matching Pursuit
#' or Orthogonal Matching Pursuit decomposition. They contain the original
#' signal, its reconstruction, parameters of the selected atoms, individual
#' selected atom waveforms, the corresponding time vector, and sampling
#' frequency.
#'
#' The plotting method displays a time-frequency (T-F) map to visualize
#' the decomposition. It is a wrapper around \code{tf_map()} with
#' \code{out_mode = "plot"}.
#'
#' @name mp-methods
#'
#' @param x An object of class \code{"mp"} or \code{"summary.mp"}.
#'   Objects of class \code{"mp"} are returned by \code{empi_execute()}
#'   or \code{mp_omp_execute()}.
#'
#' @param object An object of class \code{"mp"}.
#'
#' @param channel Channel to process and display.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}. Determines the intensity
#' with which the so-called blobs are displayed on the T-F map.
#'
#' @param freq_divide Specifies how many times the displayed frequency range in the T-F map
#' should be reduced. At high sampling rates, and when a low-pass filter with
#' a cut-off frequency much lower than the sampling frequency is used, a large part of
#' the T-F map may contain no blobs. If the sampling frequency is \code{f},
#' the maximum frequency in the T-F map will be
#' \code{ceiling(f / 2 / freq_divide)}
#' (\code{f / 2} follows the Nyquist rule). If \code{NULL}, it is determined
#' from the atom with the highest frequency \code{fmax} according to
#' \code{freq_divide = (f / 2) / fmax}.
#'
#' @param increase_factor Factor controlling the increase in the number of pixels along the
#' frequency axis. Non-negative integers such as 2, 4, 5, or 8 are typically appropriate.
#'
#' @param shortening_factor_x Usually, a value of 2 provides better visualization of atoms.
#'
#' @param shortening_factor_y Usually, a value of 2 provides better visualization of atoms.
#'
#' @param atom_centers \code{"crosses"}, \code{"numbers"}, or \code{NULL}. Determines
#' how the centres of atoms (represented as so-called blobs) are marked on the T-F map
#' (with small crosses, atom numbers, or no markers).
#'
#' @param display_grid Logical. If \code{TRUE}, grid lines are drawn.
#'
#' @param color Color of the small crosses and atom numbers.
#'
#' @param palette Palette from the list returned by \code{hcl.pals()} or the string
#' \code{"my custom palette"}.
#'
#' @param plot_signals Logical. If \code{TRUE}, the original and reconstructed signals are displayed.
#'
#' @param verbose Logical flag indicating whether progress information should be printed.
#'
#' @param ... Additional arguments. Currently ignored.
#'
#' @return
#' \code{print.mp()} and \code{print.summary.mp()} return their input
#' object invisibly.
#'
#' \code{summary.mp()} returns an object of class \code{"summary.mp"}
#' containing signal dimensions, sampling information, the total number
#' of selected atoms, the number of selected atoms per channel,
#' #' signal, residual, and explained energy information, and ranges of
#' selected atom parameters.
#'
#' \code{plot.mp()} is called for its side effect and returns no value.
#'
#' @seealso
#' \code{\link{tf_map}},
#' \code{\link{empi_execute}},
#' \code{\link{mp_omp_execute}}
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read_csv_signals(file, col_names = "ch1")
#'
#' # Execute the MP algorithm.
#' out_empi <- empi_execute(signal = signal)
#'
#' # Print and summarize the decomposition.
#' print(out_empi)
#' summary(out_empi)
#'
#' # Plot a time-frequency map based on MP atoms.
#' plot(out_empi)
#' }
#'
#' @rdname mp-methods
#' @export
plot.mp <- function(
    x,
    channel = 1,
    mode = "sqrt",
    freq_divide = NULL,
    increase_factor = 8,
    shortening_factor_x = 2,
    shortening_factor_y = 2,
    atom_centers = "crosses",
    display_grid = FALSE,
    color = "white",
    palette = "my custom palette",
    plot_signals = TRUE,
    verbose = FALSE,
    ...
) {

  # Save current graphical parameters to reset
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par), add = FALSE)

  object <- x

  if (!inherits(object, "mp")) {
    stop("'x' must be an object of class 'mp'.")
  }

  if (is.null(freq_divide)) {
    rows <- which(object$atoms$channel_id == channel)
    ff <- max(object$atoms$frequency[rows])
    freq_divide <- (object$sampling_frequency / 2) / ff
  }

  tf_map(
    x = object,
    channel = channel,
    mode = mode,
    freq_divide = freq_divide,
    increase_factor = increase_factor,
    shortening_factor_x = shortening_factor_x,
    shortening_factor_y = shortening_factor_y,
    atom_centers = atom_centers,
    display_grid = display_grid,
    color = color,
    palette = palette,
    plot_signals = plot_signals,
    out_mode = "plot",
    verbose = verbose
  )

  invisible(NULL)
}


#' @rdname mp-methods
#' @export
print.mp <- function(x, ...) {

  n_samples <- nrow(x$signal)
  n_channels <- ncol(x$signal)
  duration <- n_samples / x$sampling_frequency
  n_atoms <- nrow(x$atoms)

  signal_energy <- sum(x$signal^2, na.rm = TRUE)
  residual_energy <- sum((x$signal - x$reconstruction)^2, na.rm = TRUE)

  explained_energy <- if (signal_energy > 0) {
    1 - residual_energy / signal_energy
  } else {
    NA_real_
  }

  cat("Matching Pursuit object (class 'mp')\n")
  cat("--------------------------------------------------\n")
  cat("Samples:              ", n_samples, "\n", sep = "")
  cat("Channels:             ", n_channels, "\n", sep = "")
  cat("Sampling frequency:   ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:             ", signif(duration, 6), " s\n", sep = "")
  cat("Selected atoms:       ", n_atoms, "\n", sep = "")
  cat(
    "Explained energy:     ",
    if (is.na(explained_energy)) "NA" else paste0(round(100 * explained_energy, 2), "%"),
    "\n",
    sep = ""
  )

  invisible(x)
}

#' @rdname mp-methods
#' @export
summary.mp <- function(object, ...) {

  n_samples <- nrow(object$signal)
  n_channels <- ncol(object$signal)
  duration <- n_samples / object$sampling_frequency
  n_atoms <- nrow(object$atoms)

  # Number of selected atoms per channel
  atoms_per_channel <- table(object$atoms$channel_id)

  # Signal, residual, and explained energy
  signal_energy <- sum(object$signal^2, na.rm = TRUE)
  residual_energy <- sum(
    (object$signal - object$reconstruction)^2,
    na.rm = TRUE
  )

  explained_energy <- if (signal_energy > 0) {
    1 - residual_energy / signal_energy
  } else {
    NA_real_
  }

  # Ranges of atom parameters
  if (n_atoms > 0) {
    frequency_range <- range(object$atoms$frequency, na.rm = TRUE)
    position_range <- range(object$atoms$position, na.rm = TRUE)
    scale_range <- range(object$atoms$scale, na.rm = TRUE)
  } else {
    frequency_range <- c(NA_real_, NA_real_)
    position_range <- c(NA_real_, NA_real_)
    scale_range <- c(NA_real_, NA_real_)
  }

  out <- list(
    samples = n_samples,
    channels = n_channels,
    sampling_frequency = object$sampling_frequency,
    duration = duration,
    selected_atoms = n_atoms,
    atoms_per_channel = atoms_per_channel,
    signal_energy = signal_energy,
    residual_energy = residual_energy,
    explained_energy = explained_energy,
    frequency_range = frequency_range,
    position_range = position_range,
    scale_range = scale_range
  )

  class(out) <- "summary.mp"

  out
}


#' @rdname mp-methods
#' @export
print.summary.mp <- function(x, ...) {

  cat("Summary of Matching Pursuit object (class 'mp')\n")
  cat("--------------------------------------------------\n")
  cat("Samples:              ", x$samples, "\n", sep = "")
  cat("Channels:             ", x$channels, "\n", sep = "")
  cat("Sampling frequency:   ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:             ", signif(x$duration, 6), " s\n", sep = "")
  cat("Selected atoms:       ", x$selected_atoms, "\n", sep = "")

  cat("\nReconstruction:\n")
  cat("Original signal energy: ",
      signif(x$signal_energy, 6), "\n", sep = "")
  cat("Residual energy:        ",
      signif(x$residual_energy, 6), "\n", sep = "")
  cat("Explained energy:       ",
      if (is.na(x$explained_energy)) "NA" else paste0(round(100 * x$explained_energy, 2), "%"),
      "\n", sep = "")

  cat("\nSelected atoms per channel:\n")
  for (i in seq_along(x$atoms_per_channel)) {
    cat("Channel ", names(x$atoms_per_channel)[i], ": ",
        x$atoms_per_channel[i], "\n", sep = "")
  }

  cat("\nAtom parameters:\n")
  cat("Frequency range: ",
      paste(signif(x$frequency_range, 4), collapse = " - "),
      " Hz\n", sep = "")
  cat("Position range:  ",
      paste(signif(x$position_range, 4), collapse = " - "),
      " s\n", sep = "")
  cat("Scale range:     ",
      paste(signif(x$scale_range, 4), collapse = " - "),
      " s\n", sep = "")

  invisible(x)
}

