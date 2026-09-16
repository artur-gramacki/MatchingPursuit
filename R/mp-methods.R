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
#' (\code{f / 2} corresponds to the Nyquist frequency). If \code{NULL}, it is determined
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

  if (!inherits(x, "mp")) {
    stop("'x' must be an object of class 'mp'.")
  }

  signal <- as.matrix(x$signal)

  n_samples <- nrow(signal)
  n_channels <- ncol(signal)
  duration <- n_samples / x$sampling_frequency

  cat("Matching Pursuit object (class 'mp')\n")
  cat("--------------------------------------\n")
  cat("Samples:            ", n_samples, "\n", sep = "")
  cat("Channels:           ", n_channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(duration, 6), " s\n", sep = "")

  invisible(x)
}

#' @rdname mp-methods
#' @export
summary.mp <- function(object, ...) {

  if (!inherits(object, "mp")) {
    stop("'object' must be an object of class 'mp'.")
  }

  signal <- as.matrix(object$signal)
  reconstruction <- as.matrix(object$reconstruction)
  n_samples <- nrow(signal)
  n_channels <- ncol(signal)
  duration <- n_samples / object$sampling_frequency
  channel_names <- colnames(signal)

  if (is.null(channel_names)) {
    channel_names <- paste0("Channel ", seq_len(n_channels))
  }

  atoms_per_channel <- tabulate(object$atoms$channel_id, nbins = n_channels)
  channel_summary <- vector("list", n_channels)

  for (i in seq_len(n_channels)) {
    signal_i <- signal[, i]
    reconstruction_i <- reconstruction[, i]
    signal_energy <- sum(signal_i^2, na.rm = TRUE)
    reconstruction_energy <- sum(reconstruction_i^2, na.rm = TRUE)
    residual_energy <- sum((signal_i - reconstruction_i)^2, na.rm = TRUE)

    explained_energy <- if (signal_energy > 0) {
      1 - residual_energy / signal_energy
    } else {
      NA_real_
    }

    channel_summary[[i]] <- data.frame(
      channel = i,
      channel_name = channel_names[i],
      selected_atoms = atoms_per_channel[i],
      signal_energy = signal_energy,
      reconstruction_energy = reconstruction_energy,
      residual_energy = residual_energy,
      explained_energy = explained_energy,
      stringsAsFactors = FALSE
    )
  }

  channel_summary <- do.call(rbind, channel_summary)

  out <- list(
    samples = n_samples,
    channels = n_channels,
    sampling_frequency = object$sampling_frequency,
    duration = duration,
    channel_summary = channel_summary
  )

  class(out) <- "summary.mp"
  out
}

#' @rdname mp-methods
#' @export
print.summary.mp <- function(x, ...) {

  cat("Summary of Matching Pursuit object (class 'mp')\n")
  cat("--------------------------------------------------------\n")
  cat("Samples:            ", x$samples, "\n", sep = "")
  cat("Channels:           ", x$channels, "\n", sep = "")
  cat("Sampling frequency: ", x$sampling_frequency, " Hz\n", sep = "")
  cat("Duration:           ", signif(x$duration, 6), " s\n", sep = "")

  cat("\nPer-channel decomposition:\n\n")
  cat("Explained energy = 1 - residual energy / signal energy\n")
  cat("For MP, this measure is preferred because the reconstruction \n")
  cat("and residualare not generally orthogonal, so reconstruction \n")
  cat("energy / signal energy is not equivalent to explained energy.\n")

  for (i in seq_len(nrow(x$channel_summary))) {

    ch <- x$channel_summary[i, ]

    cat("\nChannel ", ch$channel, " (", ch$channel_name,"): ", ch$selected_atoms, " atoms\n", sep = "")
    cat("Signal energy:         ", signif(ch$signal_energy, 6), "\n", sep = "")
    cat("Reconstruction energy: ", signif(ch$reconstruction_energy, 6), "\n", sep = "")
    cat("Residual energy:       ", signif(ch$residual_energy, 6), "\n", sep = "")
    cat(
      "Explained energy:      ",
      if (is.na(ch$explained_energy)) "NA" else paste0(round(100 * ch$explained_energy, 2), "%"), "\n", sep = "")

    invisible(x)
  }
}
