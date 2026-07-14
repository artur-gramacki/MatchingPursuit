#' Creates a time-frequency map using atoms from the Matching Pursuit algorithm
#'
#' Creates a time-frequency map using atoms from the Matching Pursuit algorithm.
#' The resulting map can be: 1) displayed on the screen, 2) saved as a \code{.png} file,
#' or 3) saved as an \code{.RData} object.
#'
#' @importFrom graphics rasterImage par points text axis mtext layout plot.new plot.window box abline title
#' @importFrom grDevices hcl.colors graphics.off pdf dev.off png
#' @importFrom utils tail
#' @importFrom DescTools DrawEllipse
#' @importFrom imager as.cimg resize
#'
#' @param x An object of class \code{mp} or a path to a SQLite file created by \code{empi_execute()}.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}. Determines the intensity
#' with which the so-called blobs are displayed on the T-F map.
#'
#' @param freq_divide Specifies how many times the displayed frequency range in the T-F map
#' should be reduced. At high sampling rates, especially when a low-pass filter with
#' a cut-off frequency much lower than the sampling frequency is used, a large part of
#' the T-F map may contain no blobs. If the sampling frequency is \code{f},
#' the maximum frequency displayed in the T-F map will be \code{ceiling(f / 2 / freq_divide)}
#' (\code{f / 2} follows the Nyquist rule). If \code{NULL}, it is determined from the atom
#' with the highest frequency \code{fmax} according to \code{freq_divide = (f / 2) / fmax}.
#'
#'
#' @param increase_factor Factor controlling the increase in the number of pixels along the
#' frequency axis. Non-negative integers such as 2, 4, 5, or 8 are usually appropriate.
#'
#' @param shortening_factor_x Usually, a value of 2 provides better atom visualization.
#'
#' @param shortening_factor_y Usually, a value of 2 provides better atom visualization.
#'
#' @param display_crosses Whether small crosses should be displayed at the centres of atoms.
#'
#' @param display_atom_numbers Whether atom numbers should be displayed in the canters of atoms.
#'
#' @param display_grid Whether grid lines should be drawn.
#'
#' @param color Color of the small crosses or atom numbers.
#'
#' @param palette Palette from the list returned by the \code{hcl.pals()} function or the string
#' \code{"my custom palette"}.
#'
#' @param rev Value of the \code{rev} argument passed to the \code{hcl.colors()} function.
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
#'     \item{original_signal}{ original signal.}
#'     \item{sf}{ sampling frequency.}
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
#' file <- system.file("extdata", "sample1.db", package = "MatchingPursuit")
#' empi_class <- read_empi_db_file(file)
#'
#' # 'freq_divide' is set arbitrarily
#' out <- tf_map(
#'   x = empi_class,
#'   channel = 1,
#'   mode = "sqrt",
#'   freq_divide = 4,
#'   increase_factor= 4,
#'   display_crosses = TRUE,
#'   display_atom_numbers = FALSE,
#'   out_mode = "plot",
#' )
#'
#' # 'freq_divide' is determined based on the atom with the highest frequency
#' out <- tf_map(
#'   x = empi_class,
#'   channel = 1,
#'   mode = "sqrt",
#'   increase_factor= 4,
#'   display_crosses = TRUE,
#'   display_atom_numbers = FALSE,
#'   out_mode = "plot",
#' )
#'
tf_map <- function(
    x = NULL,
    channel,
    mode = "sqrt",
    freq_divide = NULL,
    increase_factor = 1,
    shortening_factor_x = 2,
    shortening_factor_y = 2,
    display_crosses = TRUE,
    display_atom_numbers = FALSE,
    display_grid = FALSE,
    color = "white",
    palette = 'my custom palette',
    rev = TRUE,
    out_mode = "plot",
    path = NULL,
    file_name = NULL,
    size = c(512, 512),
    draw_ellipses = FALSE,
    plot_signals = TRUE,
    write_atoms = FALSE,
    verbose = TRUE) {

  # Store
  old_par <- par("mfrow", "pty", "mai", "mgp", "las", "xaxs", "yaxs")
  on.exit(par(old_par))

  if (inherits(x, "mp")) {
    out <- x
  }

  if (!inherits(x, "mp")) {
    # check if a string is a legal path to an existing file
    if (is.character(x) &&  length(x) == 1 && !is.na(x) && file.exists(x) && !dir.exists(x)) {
      out <- read_empi_db_file(x)
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

  if (out_mode != "RData" & out_mode != "RData2")
    tf_map_resampled <- NULL

  if (palette == 'my custom palette') {
    col <-  c(
      "#000f82", "#001385", "#011789", "#011b8d", "#021f91", "#022395", "#032798", "#042b9c",
      "#042fa0", "#0533a4", "#0537a8", "#063bab", "#073faf", "#0743b3", "#0847b7", "#084bbb",
      "#094fbf", "#0a53c2", "#0a57c6", "#0b5bca", "#0b5fce", "#0c63d2", "#0d67d5", "#0d6bd9",
      "#0e6fdd", "#0e73e1", "#0f77e5", "#107be8", "#107fec", "#1183f0", "#1187f4", "#128bf8",
      "#1390fc", "#1693f8", "#1996f4", "#1c9af0", "#1f9dec", "#22a1e8", "#25a4e4", "#28a8e0",
      "#2cabdc", "#2faed8", "#32b2d4", "#35b5d0", "#38b9cc", "#3bbcc8", "#3ec0c4", "#41c3c0",
      "#45c7bc", "#48cab8", "#4bcdb4", "#4ed1b0", "#51d4ac", "#54d8a8", "#57dba4", "#5adfa0",
      "#5ee29c", "#61e598", "#64e994", "#67ec90", "#6af08c", "#6df388", "#70f784", "#73fa80",
      "#77fe7c", "#7bf978", "#7ff575", "#83f171", "#88ed6e", "#8ce96a", "#90e567", "#94e063",
      "#99dc60", "#9dd85c", "#a1d459", "#a5d055", "#aacc52", "#aec74e", "#b2c34b", "#b6bf47",
      "#bbbb44", "#bfb741", "#c3b33d", "#c7af3a", "#ccaa36", "#d0a633", "#d4a22f", "#d89e2c",
      "#dd9a28", "#e19625", "#e59121", "#e98d1e", "#ee891a", "#f28517", "#f68113", "#fa7d10",
      "#ff790d", "#fb750c", "#f8720c", "#f56f0b", "#f26c0b", "#ef690a", "#eb660a", "#e8630a",
      "#e56009", "#e25c09", "#df5908", "#db5608", "#d85308", "#d55007", "#d24d07", "#cf4a06",
      "#cc4706", "#c84306", "#c54005", "#c23d05", "#bf3a04", "#bc3704", "#b83404", "#b53103",
      "#b22e03", "#af2a02", "#ac2702", "#a82402", "#a52101", "#a21e01", "#9f1b00", "#9c1800",
      "#991500")
  } else {
    col <- hcl.colors(128, palette, rev = rev)
  }

  total_channels <- length(unique(out$atoms$channel_id))

  if (length(which(out$atoms$channel_id == channel)) == 0)
    stop("There is no channel number ", channel, ".")

  sf <- out$sf

  if (is.null(freq_divide)) {
    rows <- which(out$atoms$channel_id == channel)
    ff <- max(out$atoms$frequency[rows])
    freq_divide <- (out$sf / 2) / ff
    # cat("max atom frequency: ", ff, "\n", sep = "")
    # cat("freq_divide: ", freq_divide, "\n", sep = "")
  }

  # sf / 2: according to the Nyquist criterion
  maxf <- ceiling((sf / 2) / freq_divide)

  epochSize <- length(out$t)
  s <- epochSize / sf

  # grid size in t
  t <- seq(from = 0, to = s, length.out = epochSize)

  # grid size in sf
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
  original_signal <- out$original_signal[, channel]
  reconstruction <- out$reconstruction[, channel]
  gabors <- out$gabors[[channel]]

  # Signal energy
  o <- round(sum(original_signal^2), 2)
  r <- round(sum(reconstruction^2), 2)

  if (verbose) {
    message(
      "Channel number: ", channel, "\n",
      "Total channels: ", total_channels, "\n",
      "Number of atoms: ", length(rows), "\n",
      "Sampling frequency: ", sf, " Hz", "\n",
      "Epoch size (in points): ", epochSize, "\n",
      "Signal length (in seconds): ", s, "\n",
      "\nEnergy of the original signal:      ",o, "\n",
      "Energy of the reconstructed signal: ",r, "\n",
      "reconstruction / original %:        ", round(r / o * 100, digits = 2), "\n")
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
          at = seq(from = 0, to = ceiling(tail(t * sf, 1)), length.out = 11),
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
      ellipse <- DrawEllipse(
        x = position[n],
        y = frequency[n],
        # from Heisenberg rule: delta_t x delta_omega >= 1/2
        radius.x = (scale[n] / 2),
        radius.y = 1 / ((scale[n])),
        col = "lightgray",
        border = "black",
        plot = TRUE,
        nv = 100)

      if (display_crosses) {
        points(position[n], frequency[n] , pch = 3, col = "black", cex = 1)
      }

      if(display_atom_numbers) {
        text(position[n], frequency[n], n, col = "black", cex = 1)
      }
    }

    if (mode == "sqrt")
      A  <- sqrt(energy[n] * sf)

    if (mode == "log")
      A  <- log(energy[n] * sf)

    if (mode == "linear")
      A  <- energy[n] * sf

    # from Heisenberg rule: delta_t x delta_omega >= 1/2
    radius_x <- (scale[n] / 2) / shortening_factor_x
    radius_y <- 1 / ((scale[n])) / shortening_factor_y

    x0 <- position[n]
    y0 <- frequency[n]
    sx <- radius_x
    sy <- radius_y

    # 2D Gaussian
    z <- with(grid,
              A * exp(-((x - x0)^2 / (2 * sx^2) +
                        (y - y0)^2 / (2 * sy^2))))

    # convert to a matrix, because image() requires it
    z_mtx <- matrix(z, nrow = length(t), ncol = length(y))
    tf_map <- tf_map + z_mtx

  } # for (n in 1:num_atoms)

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

    z_col <- col[cut(tf_map, breaks = 129)]
    # image() and rasterImage() differ in the orientation of the Y axis.
    # Often you also need to do the following:
    rot90 <- function(m) t(m)[ncol(m):1, ]
    tf_map_rot90 <- rot90(matrix(z_col, nrow(tf_map)))
    plot.new()
    plot.window(range(t), range(y))
    rasterImage(tf_map_rot90, 0, 0, tail(t, 1), tail(y, 1))
    main_txt <- paste("channel: ", channel, "/", total_channels, ", sampling rate: ", sf, " Hz", sep = "")
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
    if (display_atom_numbers) {
      for (n in 1:num_atoms) {
        text(position[n], frequency[n], n, col = color, cex = 1)
      }
    }

    # We display small crosses in the centres of atoms
    for (n in 1:num_atoms) {
      if (display_crosses) {
        points(position[n], frequency[n], pch = 3, col = color, cex = 0.8)
      }
    }

    if (display_grid)
      grid(col = "grey")

    if (plot_signals) {
      sig_range <- range(original_signal)
      xx <- seq(from = 0, to = epochSize / sf, length.out =  epochSize)
      plot(x = xx, original_signal, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, ylim = sig_range,
           main = "Original signal", panel.first = grid())
      abline(h = 0, col = "blue")

      plot(x = xx, reconstruction, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, ylim = sig_range,
           main = "Reconstructed signal", panel.first = grid())
      abline(h = 0, col = "blue")
    }

  } # if (out_mode == "plot")

  if (out_mode == "file") {
    graphics.off()
    png(file_name, width = size[1], height = size[2], pointsize = 18)
    par(pty = "m", mai = c(0, 0, 0, 0))
    graphics::image(x = t, y = y, z = tf_map, col = col)
    if (display_crosses) points(position, frequency, pch = 3, col = color, cex = 0.8)
    if (display_atom_numbers) {
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

  list(
    atoms = out$atoms,
    gabor_functions = gabors,
    reconstruction = reconstruction,
    original_signal = original_signal,
    sf = sf,
    grid_size_t = t,
    grid_size_f = y,
    epochSize = epochSize,
    number_of_secs = s,
    tf_map = tf_map,
    tf_map_resampled = tf_map_resampled,
    channel = channel,
    freq_divide = freq_divide)
}

