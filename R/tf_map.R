#' Creates a time-frequency map using atoms from the Matching Pursuit algorithm
#'
#' Creates a time-frequency map using atoms from the Matching Pursuit algorithm.
#' The resulting map can be: 1) displayed on the screen, 2) saved as a \code{.png} file,
#' or 3) saved as an \code{.RData} object.
#'
#' The function also computes basic reconstruction-quality measures for the
#' selected channel, including signal energy, residual energy, and the proportion of explained
#' signal energy.
#'
#' @importFrom graphics rasterImage par points text axis mtext layout plot.new plot.window box abline title
#' @importFrom grDevices hcl.colors graphics.off pdf dev.off png
#' @importFrom utils tail
#' @importFrom imager as.cimg resize
#'
#' @param x An object of class \code{mp} or a path to a SQLite file created by \code{empi_execute()}.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"} (default), \code{"linear"}, or \code{"energy"}.
#'   Controls the amplitude assigned to each Gaussian blob.
#'   \code{"sqrt"} uses \code{sqrt(energy * sampling_frequency)};
#'   \code{"linear"} uses \code{energy * sampling_frequency};
#'   \code{"energy"} normalizes the integral of each untruncated blob
#'   over the whole time-frequency plane to its assigned atom energy.
#'
#' @param display_scale \code{"linear"} (default) or \code{"db"}.
#'   Applies dB scaling after all blobs have been summed, for plotting only.
#'   Does not modify the returned \code{tf_map} or saved RData matrices.
#'
#' @param db_range Positive number specifying the displayed dynamic range
#'   in dB (default 20); used only if \code{display_scale = "db"}.
#'
#' @param freq_divide Specifies how many times the displayed frequency range in the T-F map
#' should be reduced. At high sampling rates, especially when a low-pass filter with
#' a cut-off frequency much lower than the sampling frequency is used, a large part of
#' the T-F map may contain no blobs. If the sampling frequency is \code{f},
#' the maximum frequency displayed in the T-F map will be \code{ceiling(f / 2 / freq_divide)}
#' (\code{f / 2} follows the Nyquist rule). If \code{NULL}, it is determined from the atom
#' with the highest frequency \code{fmax} according to \code{freq_divide = (f / 2) / fmax}.
#'
#' @param increase_factor Factor controlling the increase in the number of pixels along the
#' frequency axis. Non-negative integers such as 2, 4, 5, or 8 are usually appropriate.
#'
#' @param shortening_factor_x Positive factor narrowing the Gaussian blobs along time;
#'   the default of 2 is a visualization choice, not an estimated physical uncertainty.
#'
#' @param shortening_factor_y Positive factor narrowing the Gaussian blobs along frequency;
#'   the default of 2 is a visualization choice, not an estimated physical uncertainty.
#'
#' @param atom_centers \code{"crosses"}, \code{"numbers"}, or \code{NULL}. Determines
#' how the centres of atoms (represented as so-called blobs) are marked on the T-F map
#' (with small crosses, atom numbers, or no markers).
#'
#' @param display_grid Whether grid lines should be drawn.
#'
#' @param color Color of the small crosses or atom numbers.
#'
#' @param palette Palette from the list returned by the \code{hcl.pals()} function or the string
#' \code{"my custom palette"}.
#'
#' @param reverse_palette Value of the \code{rev} argument passed to the \code{hcl.colors()} function.
#'
#' @param out_mode One of the following:
#'   \itemize{
#'      \item \code{"plot"} - draws a T-F map on the screen.
#'      \item \code{"file"} - saves a T-F map to the file \code{file_name} (as a \code{png} file).
#'      \item \code{"RData"} - saves the T-F map of size \code{size} to \code{file_name}
#'      (as an R matrix); resampling is performed using the \code{imager::resize()} function.
#'      \item \code{"RData2"} - saves the T-F map of size \code{size} to \code{file_name}
#'      (as an R matrix); resampling is performed using the \code{raster::resample()} function.
#'    }
#'
#' @param path Path where \code{png}, \code{RData}, or \code{pdf} files will be written. If \code{NULL},
#' files will be written to the cache directory.
#'
#' @param file_name Name of the \code{png} file (if \code{out_mode = "file"}) or name of the
#' \code{RData} file (if \code{out_mode = "RData"} or \code{out_mode = "RData2"}).
#'
#' @param size Size of the \code{png} file in pixels (if \code{out_mode = "file"}) or size of
#' the T-F matrix (if \code{out_mode = "RData"} or \code{out_mode = "RData2"}).
#'
#' @param draw_ellipses Intended for testing only. Can be set to \code{TRUE} to display
#' the effect. Works correctly only if \code{out_mode = "plot"}.
#'
#' @param plot_signals Whether the original and reconstructed signals should also be displayed.
#'
#' @param write_atoms If \code{TRUE}, writes all atom plots to the \code{Atoms.pdf} file
#' (in the cache directory or in a user-specified directory, depending on \code{path}).
#'
#' @param verbose Logical flag indicating whether progress information should be printed.
#'
#' @section Best practices:
#' \itemize{
#'   \item For an exploratory, visually balanced map, start with
#'     \code{mode = "sqrt", display_scale = "linear"}.
#'     This reduces the contrast between weak and strong atoms; the map is not
#'     an energy-normalized time-frequency density.
#'
#'   \item To represent assigned atom energies by the integrals of Gaussian
#'     blobs, use \code{mode = "energy"}. The normalization holds for
#'     each complete blob; cropping at time or frequency boundaries can remove
#'     part of its displayed integral. For MP/OMP, the interpretation of the
#'     supplied atom energies depends on how they were computed upstream.
#'
#'   \item Use \code{display_scale = "db"} to make weak structures
#'     more visible. It computes \code{10 * log10(tf_map / max(tf_map))}
#'     and clips values below \code{-db_range} dB. Try 20 dB for a
#'     selective display and 40 dB to reveal more weak structures.
#'     \code{db_range} has no effect with \code{display_scale = "linear"}.
#'
#'   \item \code{mode = "linear"} is a legacy amplitude rule proportional
#'     to atom energy. At fixed shortening factors it differs from
#'     \code{mode = "energy"} only by a global multiplier.
#'
#'   \item When comparing MP, OMP, signals, or recordings, keep the mode,
#'     Gaussian widths, frequency range, and palette fixed. The dB display
#'     is normalized separately to each map's maximum, so colors across
#'     different maps do not indicate equal absolute intensities.
#'
#'   \item The returned \code{tf_map} contains the summed, untransformed
#'     Gaussian map for the chosen \code{mode}; it is not a spectrogram.
#'     Saved RData matrices are additionally resized and scaled to [0, 1].
#' }
#'
#' @return Depending on the \code{out_mode} parameter, the function:
#'    \itemize{
#'      \item displays the time-frequency map on the screen
#'      \item saves the time-frequency map as a \code{.png} file
#'      \item saves the time-frequency map as a \code{.RData} file
#'   }
#' Regardless of the output mode, the function also returns:
#'
#'     \item{gabor_functions}{All Gabor functions.}
#'     \item{reconstruction}{Reconstructed signal.}
#'     \item{signal}{ Original signal.}
#'     \item{signal_energy}{Energy of the original signal for the selected channel.}
#'     \item{reconstruction_energy}{Energy of the reconstructed signal for the selected channel.}
#'     \item{residual_energy}{Energy of the reconstruction residual.}
#'     \item{explained_energy}{Proportion of the original signal energy explained by the reconstruction,
#'     \code{1 - sum((signal - reconstruction)^2) / sum(signal^2)}.}
#'     \item{sampling_frequency}{Sampling frequency.}
#'     \item{grid_size_t}{Grid size along the time axis.}
#'     \item{grid_size_f}{Grid size along the frequency axis.}
#'     \item{epochSize}{Epoch size in samples.}
#'     \item{number_of_secs}{Signal length in seconds.}
#'     \item{tf_map}{Time-frequency map.}
#'     \item{tf_map_resampled}{Resampled time-frequency map
#'     (if \code{out_mode = "RData"} or \code{out_mode = "RData2"}; otherwise \code{NULL}).}
#'     \item{channel}{Processed channel number.}
#'     \item{freq_divide}{Frequency division factor.}
#'
#' @export
#'
#' @examples
#' # Read signal and XML dictionary
#' file <- system.file(
#'   "extdata", "sample1.csv",
#'   package = "MatchingPursuit"
#' )
#'
#' signal <- read_csv_signals(file)
#'
#' xml_file <- system.file(
#'   "extdata", "sample1.xml",
#'   package = "MatchingPursuit"
#' )
#'
#' # Perform OMP decomposition
#' sample1_empi_out <- mp_omp_execute(
#'   mode = "omp",
#'   signal = signal,
#'   dictionary = xml_file,
#'   n_nonzero_coefs = 16,
#'   verbose = TRUE
#' )
#'
#' # Example 1: Default visualization
#' # Square-root amplitude scaling and linear display.
#' out <- tf_map(
#'   x = sample1_empi_out,
#'   channel = 1,
#'   mode = "sqrt",
#'   display_scale = "linear",
#'   freq_divide = 4
#' )
#'
#' # Example 2: Automatic frequency range
#' # The displayed frequency range is determined from the
#' # highest frequency among the selected atoms.
#' out <- tf_map(
#'   x = sample1_empi_out,
#'   channel = 1,
#'   mode = "sqrt",
#'   display_scale = "linear",
#'   freq_divide = NULL
#' )
#'
#' # Example 3: Energy-normalized Gaussian blobs
#' # With a linear display, strong atoms dominate;
#' # weak atoms may be less visible.
#' out <- tf_map(
#'   x = sample1_empi_out,
#'   channel = 1,
#'   mode = "energy",
#'   display_scale = "linear",
#'   freq_divide = 4
#' )
#'
#' # Example 4: Logarithmic display
#' # Enhances the visibility of weaker structures.
#' # Values below -20 dB are mapped to the background color.
#' out <- tf_map(
#'   x = sample1_empi_out,
#'   channel = 1,
#'   mode = "energy",
#'   display_scale = "db",
#'   db_range = 20,
#'   freq_divide = 4
#' )
#'
#' # Example 5: Wider dynamic range and alternative palette
#' # Larger shortening factors produce more compact blobs.
#' # Atom numbers identify the selected dictionary elements.
#' out <- tf_map(
#'   x = sample1_empi_out,
#'   channel = 1,
#'   mode = "energy",
#'   display_scale = "db",
#'   db_range = 40,
#'   freq_divide = 4,
#'   shortening_factor_x = 4,
#'   shortening_factor_y = 4,
#'   palette = "Inferno",
#'   reverse_palette = FALSE,
#'   atom_centers = "numbers"
#' )
#'
tf_map <- function(
    x = NULL,
    channel,
    mode = "sqrt",
    display_scale = "linear",
    db_range = 20,
    freq_divide = NULL,
    increase_factor = 4,
    shortening_factor_x = 2,
    shortening_factor_y = 2,
    atom_centers = "crosses",
    display_grid = FALSE,
    color = "white",
    palette = "my custom palette",
    reverse_palette = TRUE,
    out_mode = "plot",
    path = NULL,
    file_name = NULL,
    size = c(512, 512),
    draw_ellipses = FALSE,
    plot_signals = TRUE,
    write_atoms = FALSE,
    verbose = FALSE) {

  mode <- match.arg(mode, c("sqrt", "linear", "energy"))
  display_scale <- match.arg(display_scale, c("linear", "db"))
  if (length(db_range) != 1L || !is.numeric(db_range) ||
      !is.finite(db_range) || db_range <= 0) {
    stop("'db_range' must be a positive finite number.")
  }

  # Store
  old_par <- par("mfrow", "pty", "mai", "mgp", "las", "xaxs", "yaxs")
  on.exit(par(old_par))

  if (inherits(x, "mp")) {
    out <- x
  }

  if (!inherits(x, "mp")) {
    # check if a string is a legal path to an existing file
    if (is.character(x) &&  length(x) == 1 && !is.na(x) && file.exists(x) && !dir.exists(x)) {
      out <- read_empi_db(x)
    } else {
      stop("'x' must be an object of class 'mp' or path to an SQLite file.")
    }
  }

  if (out_mode != "plot" && out_mode != "file" && out_mode != "RData" && out_mode != "RData2")
    stop("Incorrect value for 'out_mode' parameter.'")

  if (is.null(file_name)) {
    if  (out_mode == "file") fn <- "TFmap.png"
    if  (out_mode == "RData" || out_mode == "RData2") fn <- "TFmap.RData"
  } else {
    fn <- file_name
  }

  if (out_mode != "plot") {
    if (is.null(path)) {
      dest_dir <- tools::R_user_dir("MatchingPursuit", "cache")
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      file_name <- file.path(dest_dir, fn)
    } else {
      if (!dir.exists(path)) {
        ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(path)) {
          stop("Cannot create directory '", path, "'.")
        }
      }
      file_name <- file.path(path, fn)
    }
  }

  if (write_atoms) {
    sys_time <- format(Sys.time(), "%Y-%m-%d %H-%M-%S")
    atm <- paste("Atoms_", sys_time, ".pdf", sep = "")

    if (is.null(path)) {
      dest_dir <- tools::R_user_dir("MatchingPursuit", "cache")
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      atoms_file_name <- file.path(dest_dir, atm)
    } else {
      if (!dir.exists(path)) {
        ok <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(path)) {
          stop("Cannot create directory '", path, "'.")
        }
      }
      atoms_file_name <- file.path(path, atm)
    }
  }

  if (out_mode != "RData" && out_mode != "RData2")
    tf_map_resampled <- NULL

  if (palette == 'my custom palette') {
    col <- grDevices::colorRampPalette(
      c("#000F82", "#1F5FD1", "#3FA9F5", "#8BE9FD", "#D9FF88", "#FFD166", "#F77F00", "#991500"
      ),
      space = "Lab"
    )(256)

  } else {
    col <- hcl.colors(256, palette, rev = reverse_palette)
  }


  total_channels <- length(unique(out$atoms$channel_id))

  if (length(which(out$atoms$channel_id == channel)) == 0)
    stop("There is no channel number ", channel, ".")

  sampling_frequency <- out$sampling_frequency

  if (is.null(freq_divide)) {
    rows <- which(out$atoms$channel_id == channel)
    ff <- max(out$atoms$frequency[rows])
    freq_divide <- (sampling_frequency / 2) / ff
    # cat("max atom frequency: ", ff, "\n", sep = "")
    # cat("freq_divide: ", freq_divide, "\n", sep = "")
  }

  # sampling_frequency / 2: according to the Nyquist criterion
  maxf <- ceiling((sampling_frequency / 2) / freq_divide)

  epochSize <- length(out$t)
  s <- epochSize / sampling_frequency

  # grid size in t
  t <- seq(from = 0, to = s, length.out = epochSize)

  # grid size in sampling_frequency
  y <- seq(from = 0, to = maxf, length.out = maxf * increase_factor)

  # t-f map
  tf_map <- matrix(0, nrow = epochSize, ncol = maxf * increase_factor)

  grid <- expand.grid(x = t, y = y)

  # number of atoms
  rows <- which(out$atoms$channel_id == channel)
  num_atoms <- length(rows)

  # atoms params
  energy <- out$atoms$energy[rows]
  scale <- out$atoms$scale[rows]
  position <- out$atoms$position[rows]
  frequency <- out$atoms$frequency[rows]
  signal <- out$signal[, channel]
  reconstruction <- out$reconstruction[, channel]
  gabors <- out$gabors[[channel]]

  # Signal energy
  signal_energy <- sum(signal^2, na.rm = TRUE)
  reconstruction_energy <- sum(reconstruction^2, na.rm = TRUE)

  residual_energy <- sum((signal - reconstruction)^2, na.rm = TRUE)
  explained_energy <- if (signal_energy > 0) {
    1 - residual_energy / signal_energy
  } else {
    NA_real_
  }

  if (verbose) {
    message(
      "Channel number:             ", channel, "\n",
      "Total channels:             ", total_channels, "\n",
      "Number of atoms:            ", length(rows), "\n",
      "Sampling frequency:         ", sampling_frequency, " Hz\n",
      "Epoch size (in points):     ", epochSize, "\n",
      "Signal length (in seconds): ", s, "\n\n",

      "Signal energy:              ", round(signal_energy, 4), "\n",
      "Reconstruction energy:      ", round(reconstruction_energy, 4), "\n",
      "Residual energy:            ", round(residual_energy, 4), "\n",
      "Explained_energy:           ", round(100 * explained_energy, 4),"%"
    )
  }

  if(write_atoms) {
    graphics.off()
    pdf(atoms_file_name, width = 15, height = length(rows) * 0.6)
    message("Atom plots saved in '", atoms_file_name, "'")

    nn <- num_atoms
    # mai: c(bottom, left, top, right)
    par(mfrow = c(nn, 1), pty = "m", mai = c(0.05, 4, 0.0, 0.1), mgp = c(0, 0, 0), las = 1)

    for (m in 1:num_atoms) {
      if (m %% 2 == 0) cc = "blue" else cc = "red"
      if (m == 1) {
        plot(gabors[, m], xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "l", bty = "n", col = cc)
        lab <- seq(from = 0, to = ceiling(tail(t, 1)), length.out = 11)
        axis(
          side = 1, las = 1, cex.axis = 0.9,
          at = seq(from = 0, to = ceiling(tail(t * sampling_frequency, 1)), length.out = 11),
          labels = c(formatC(lab, format = "f", digits = 2))
        )
      } else {
        plot(gabors[, m], xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "l", bty = "n", col = cc)
      }

      txt <- paste(
        "A", m,
        ", sf=",
        round(frequency[m], 2), "Hz",
        ", t=",
        round(position[m], 2), "s",
        ", sd=",
        round(scale[m], 2), "s",
        ", E=", round(energy[m], 3),
        sep = "")
      mtext(txt, side = 2, line = 0, las = 1, cex = 1)
    }
    dev.off()
  } # if(write_atoms)


  # Empty chart on which the ellipses will appear
  if (draw_ellipses && out_mode == "plot") {
    par(mfrow = c(1, 1), pty = "m", mai = c(0.9, 0.9, 0.2, 0.4))
    plot(0, xlim = c(0, tail(t, 1)), ylim = c(0, tail(y, 1)), type = "n", las = 1,
         xlab = "Time [s]", ylab = "Frequency [Hz]", yaxs = "i", xaxs = "i")
  }

  for (n in 1:num_atoms) {

    if (draw_ellipses && out_mode == "plot") {
      # ellipse <- DrawEllipse(
      #   x = position[n],
      #   y = frequency[n],
      #   # from Heisenberg rule: delta_t x delta_omega >= 1/2
      #   radius.x = (scale[n] / 2),
      #   radius.y = 1 / ((scale[n])),
      #   col = "lightgray",
      #   border = "black",
      #   plot = TRUE,
      #   nv = 100)
      #
      # To avoid loading DescTools (I use only one function from it, DrawEllipse).
      theta.inc <- 2 * pi / 100
      theta <- seq(0, 2 * pi - theta.inc, by = theta.inc)

      graphics::polygon(
        x = position[n] + (scale[n] / 2) * cos(theta),
        y = frequency[n] + (1 / scale[n]) * sin(theta),
        col = "lightgray",
        border = "black"
      )

      if (identical(atom_centers, "crosses")) {
        points(position[n], frequency[n] , pch = 3, col = "black", cex = 1)
      }

      if (identical(atom_centers, "numbers")) {
        text(position[n], frequency[n], n, col = "black", cex = 1)
      }
    }

    # from Heisenberg rule: delta_t x delta_omega >= 1/2
    radius_x <- (scale[n] / 2) / shortening_factor_x
    radius_y <- 1 / ((scale[n])) / shortening_factor_y

    x0 <- position[n]
    y0 <- frequency[n]
    sx <- radius_x
    sy <- radius_y

    if (mode == "sqrt")
      A <- sqrt(energy[n] * sampling_frequency)
    if (mode == "linear")
      A <- energy[n] * sampling_frequency
    if (mode == "energy")
      A <- energy[n] / (2 * pi * sx * sy)

    # 2D Gaussian
    z <- with(grid,
              A * exp(-((x - x0)^2 / (2 * sx^2) +
                        (y - y0)^2 / (2 * sy^2))))

    # convert to a matrix, because image() requires it
    z_mtx <- matrix(z, nrow = length(t), ncol = length(y))
    tf_map <- tf_map + z_mtx

  } # for (n in 1:num_atoms)

  # Display transformation only. Keep tf_map in its original units.
  tf_map_display <- tf_map

  if (display_scale == "db") {
    if (any(!is.finite(tf_map)) || any(tf_map < 0)) {
      stop("dB display requires a finite, non-negative map. Use mode = 'energy', 'sqrt', or 'linear'.")
    }
    peak <- max(tf_map)
    if (peak > 0) {
      tf_map_display <- 10 * log10(pmax(tf_map / peak, 10^(-db_range / 10)))
    } else {
      tf_map_display[] <- -db_range
    }
  }

  if (out_mode == "plot") {
    if (plot_signals) {
      grid_matrix <- cbind(c(1, 1, 1, 2, 3))
      layout(grid_matrix, widths = c(1, 1, 1), heights = c(2, 1, 1))
      # mai: c(bottom, left, top, right)
      par(pty = "m", mai = c(0.4, 0.7, 0.2, 0.4), xaxs = "i", yaxs = "i")
    } else {
      par(mfrow = c(1, 1), pty = "m", mai = c(0.9, 0.9, 0.2, 0.4), xaxs = "i", yaxs = "i")
    }

    # Drawing with graphics::image() is very slow, especially for large matrices.
    # Graphics::rasterImage() is much faster.

    n_col <- length(col)
    z_col <- col[cut(
      tf_map_display,
      breaks = if (display_scale == "db") seq(-db_range, 0, length.out = n_col + 1L) else n_col,
      include.lowest = TRUE
    )]

    # image() and rasterImage() differ in the orientation of the Y axis.
    # Often you also need to do the following:
    rot90 <- function(m) t(m)[ncol(m):1, ]
    tf_map_rot90 <- rot90(matrix(z_col, nrow(tf_map)))
    plot.new()
    plot.window(range(t), range(y))
    rasterImage(tf_map_rot90, 0, 0, tail(t, 1), tail(y, 1))
    main_txt <- paste("channel: ", channel, "/", total_channels, ", sampling rate: ", sampling_frequency, " Hz", sep = "")
    title(main_txt)

    lab <- seq(from = 0, to = ceiling(tail(t, 1)), length.out = 11)
    axis(
      side = 1, las = 1, cex.axis = 0.9,
      at = seq(from = 0, to = ceiling(tail(t, 1)), length.out = 11),
      labels = c(formatC(lab, format = "f", digits = 2))
    )

    lab <- seq(from = 0, to = ceiling(tail(y, 1)), length.out = 11)
    axis(
      side = 2, las = 1,  cex.axis = 0.9,
      at = seq(from = 0, to = ceiling(tail(y, 1)), length.out = 11),
      labels = c(formatC(lab, format = "f", digits = 2))
    )

    box()
    mtext("Time [s]", side = 1, line = 2, cex = 0.8)
    mtext("Frequency [Hz]", side = 2, line = 3.5, cex = 0.8)

    # At the centres of the atoms, the atom numbers
    if (identical(atom_centers, "numbers")) {
      for (n in 1:num_atoms) {
        text(position[n], frequency[n], n, col = color, cex = 1)
      }
    }

    # We display small crosses in the centres of atoms
    for (n in 1:num_atoms) {
      if (identical(atom_centers, "crosses")) {
        points(position[n], frequency[n], pch = 3, col = color, cex = 0.8)
      }
    }

    if (display_grid)
      grid(col = "grey")

    if (plot_signals) {
      sig_range <- range(signal)
      xx <- seq(from = 0, to = epochSize / sampling_frequency, length.out =  epochSize)
      plot(x = xx, signal, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, ylim = sig_range,
           main = "Original signal", panel.first = grid())
      abline(h = 0, col = "blue")
      main_txt <- paste0("Reconstructed signal (explained energy = ", round(100 * explained_energy, 2),"%)")
      plot(x = xx, reconstruction, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, ylim = sig_range,
           main = main_txt, panel.first = grid())
      abline(h = 0, col = "blue")
    }

  } # if (out_mode == "plot")

  if (out_mode == "file") {
    graphics.off()
    png(file_name, width = size[1], height = size[2], pointsize = 18)
    par(pty = "m", mai = c(0, 0, 0, 0))
    graphics::image(x = t, y = y, z = tf_map_display, col = col,
                    zlim = if (display_scale == "db") c(-db_range, 0) else range(tf_map_display))
    if (identical(atom_centers, "crosses")) points(position, frequency, pch = 3, col = color, cex = 0.8)
    if (identical(atom_centers, "numbers")) {
      for (n in 1:num_atoms) {
        text(position[n], frequency[n], n, col = color, cex = 0.8)
      }
    }

    dev.off()
    message("PNG file saved in '", file_name, "'")
  }

  if (out_mode == "RData") {
    im <- as.cimg(tf_map)
    im_resampled <- imager::resize(im, size_x = size[1], size_y = size[2], interpolation_type = 3)
    tf_map_resampled <- as.matrix(im_resampled)

    # Rescaling to the range 0-1.
    # Protect against a situation where a zero appears in the denominator.
    if (max(tf_map_resampled) - min(tf_map_resampled) == 0) {
      tf_map_resampled <- matrix(0, size[1], size[2])
    } else {
      tf_map_resampled <- (tf_map_resampled - min(tf_map_resampled)) / (max(tf_map_resampled) - min(tf_map_resampled))
    }
    save(tf_map_resampled, file = file_name)
    message("RData file saved in '", file_name, "'")
  }

  if (out_mode == "RData2") {
    zz  <- tf_map
    rr <- raster::raster(nrow = ncol(zz), ncol = nrow(zz)) # # this is how it should be: nrow = ncol(zz), ncol = nrow(zz)
    rr[] <- t(zz)
    tt <- raster::raster(ncol = size[1], nrow = size[2])
    tt <- raster::resample(rr, tt)
    tf_map_resampled <- matrix(tt@data@values, size[1], size[2])

    # Rescaling to the range 0-1.
    # Protect against a situation where a zero appears in the denominator.
    if (max(tf_map_resampled) - min(tf_map_resampled) == 0) {
      tf_map_resampled <- matrix(0, size[1], size[2])
    } else {
      tf_map_resampled <- (tf_map_resampled - min(tf_map_resampled)) / (max(tf_map_resampled) - min(tf_map_resampled))
    }

    save(tf_map_resampled, file = file_name)
    message("RData file saved in '", file_name, "'")
  } # if (out_mode == "RData2")

  # Restore
  par(old_par)

  invisible(
    list(
      atoms = out$atoms,
      gabor_functions = gabors,
      reconstruction = reconstruction,
      signal = signal,
      sampling_frequency = sampling_frequency,
      signal_energy = signal_energy,
      reconstruction_energy = reconstruction_energy,
      residual_energy = residual_energy,
      explained_energy  = explained_energy,
      grid_size_t = t,
      grid_size_f = y,
      epochSize = epochSize,
      number_of_secs = s,
      tf_map = tf_map,
      tf_map_resampled = tf_map_resampled,
      channel = channel,
      freq_divide = freq_divide
    )
  )
}

