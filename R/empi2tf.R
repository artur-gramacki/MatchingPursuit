#' Creates a time-frequency map using atoms from the Matching Pursuit algorithm.
#'
#' The map can be displayed on the screen, saved to a \code{png} file, or saved as an RData object (as a matrix).
#'
#' @importFrom graphics rasterImage par points text axis mtext layout plot.new plot.window box abline
#' @importFrom grDevices hcl.colors graphics.off pdf dev.off png
#' @importFrom utils tail
#' @importFrom DescTools DrawEllipse
#' @importFrom imager as.cimg resize
#' @importFrom raster resample
#'
#' @param db.file The SQLite file created after executing the \code{empi.execute} function. In this case, the \code{db.list} parameter must be NULL.
#'
#' @param db.list The list created after executing the \code{empi.execute} function. In this case, the \code{db.file} parameter must be NULL.
#'
#' @param channel Channel from the SQLite file to process.
#'
#' @param mode \code{"sqrt"}, \code{"log"}, or \code{"linear"}.
#'
#' @param freqDivide For setting frequency range. For example, when sampling frequency is \code{f=256Hz}, the maximum frequency is \code{f/2} (Nyquist rule) and the limited the frequency is  \code{f/2/freqDivide}.
#'
#' @param increaseFactor Factor of increasing the number of pixels in the f-axis, the most sensible are non-negative integers (e.g. 2, 4, 5, 8).
#'
#' @param shorteningFactor.x Usually, for better visualization of atoms, a value of 2 will be appropriate.
#'
#' @param shorteningFactor.y Usually, for better visualization of atoms, a value of 2 will be appropriate.
#'
#' @param displayCrosses Whether small crosses should be displayed in the canters of atoms.
#'
#' @param displayAtomNumbers Whether atom numbers should be displayed in the canters of atoms.
#'
#' @param displayGrid Whether to draw grid lines.
#'
#' @param crossesColor Colour of small crosses.
#'
#' @param palette Palette from the list returned by \code{hcl.pals} function or the string \code{"my custom palette"}.
#'
#' @param rev \code{rev} param in \code{hcl.colors} function.
#'
#' @param outMode One of the following:
#'   \itemize{
#'      \item \code{"plot"} - draws a TF map on the screen.
#'      \item \code{"file"} - saves a TF map to file \code{fileName} (as png file).
#'      \item \code{"RData"} - saves the TF map of \code{fileSize} in the \code{fileName} (as R's matrix) resampling using \code{imager::resize} function.
#'      \item \code{"RData2"} - saves the TF map of \code{fileSize} in the \code{fileName} (as R's matrix) resampling using \code{raster::resample} function.
#'    }
#'
#' @param fileName Name of the png file.
#'
#' @param fileSize File size in pixels.
#'
#' @param drawEllipses Only for testing.
#'
#' @param plotSignals Whether the original and reconstructed signals should also be displayed.
#'
#' @param plotAtoms If TRUE, plot all atoms into \code{Atoms.pdf} file.
#'
#' @return Depending on the \code{outMode} parameter the function returns:
#'    \itemize{
#'    \item a time-Frequency map plotted on the screen.
#'    \item a time-Frequency map saved in a png file.
#'    \item a time-Frequency map saved as .RData file.
#'   }
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "sample2.db", package = "MatchingPursuit")
#'
#' out <- empi2tf(
#'   db.file = file,
#'   channel = 1,
#'   mode = "sqrt",
#'   freqDivide = 1,
#'   increaseFactor= 4,
#'   shorteningFactor.x = 2,
#'   shorteningFactor.y = 2,
#'   displayCrosses = TRUE,
#'   displayAtomNumbers = FALSE,
#'   outMode = "plot"
#' )
#'

empi2tf <- function(
    db.file = NULL,
    db.list = NULL,
    channel,
    mode = "sqrt",
    freqDivide = 1,
    increaseFactor= 1,
    shorteningFactor.x = 2,
    shorteningFactor.y = 2,
    displayCrosses = TRUE,
    displayAtomNumbers = FALSE,
    displayGrid = FALSE,
    crossesColor = "white",
    palette = 'my custom palette',
    rev = TRUE,
    outMode = "file",
    fileName = "sample.png",
    fileSize = c(512, 512),
    drawEllipses = FALSE,
    plotSignals = TRUE,
    plotAtoms = FALSE) {

  if (outMode != "plot" & outMode != "file" & outMode != "RData" & outMode != "RData2")
    stop("\n--> Incorrect value for 'outMode' parameter.' <--")

  if (is.null(db.file) & is.null(db.list))
    stop("\n--> Specify input as SQLite file _OR_ a list returned by the 'empi.execute' function. <--")

  if (!is.null(db.file) & !is.null(db.list))
    stop("\n--> Specify input as SQLite file _OR_ a list returned by the 'empi.execute' function. <--")

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

  if (!is.null(db.file)) {
    out <- read.empi.db.file(db.file)
  }

  if (!is.null(db.list)) {
    out <- db.list
  }

  if (length(which(out$atoms$channel_id == channel)) == 0)
    stop("\n--> There is no channel number ", channel, ". <--")

  f <- out$f

  # according to the Nyquist criterion
  maxf <- round((f / 2) / freqDivide)

  epochSize <- length(out$t)
  s <- epochSize / f

  # grid size in t
  t <- seq(from = 0, to = s, length.out = epochSize)

  # grid size in f
  y <- seq(from = 0, to = maxf, length.out = maxf * increaseFactor)

  # t-f map
  tf.map <- matrix(0, nrow = epochSize, ncol = maxf * increaseFactor)

  grid <- expand.grid(x = t, y = y)

  # number of atoms
  rows <- which(out$atoms$channel_id == channel)
  num.atoms <- length(rows)

  # atoms params
  energy <- out$atoms$energy[rows]
  scale <- out$atoms$scale[rows]
  position <- out$atoms$position
  frequency <- out$atoms$frequency[rows]
  original.signal <- out$original.signal[, channel]
  reconstruction <- out$reconstruction[, channel]
  gabors <- out$gabors[[channel]]

  cat("Channel: ", channel, "\n", sep = "")
  cat("Number of atoms: ", length(rows), "\n", sep = "")
  cat("Sampling rate: ", f, "\n", sep = "")
  cat("Epoch size (in points): ", epochSize, "\n", sep = "")
  cat("Signal length (in seconds): ", s, "\n", sep = "")

  # Empty chart on which the ellipses will appear
  if (drawEllipses) {
    par(mfrow = c(1, 1), pty = "m")
    par(mai = c(0.9, 0.9, 0.2, 0.4))
    plot(0, xlim = c(0, tail(t, 1)), ylim = c(0, tail(y, 1)), type = "n", las = 1,
         xlab = "Time [s]", ylab = "Frequency [Hz]", yaxs = "i", xaxs = "i")
  }

  for (n in 1:num.atoms) {

    if (drawEllipses) {
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

      if (displayCrosses) {
        points(position[n], frequency[n] , pch = 3, col = "black", cex = 1)
      }

      if(displayAtomNumbers) {
        text(position[n], frequency[n], n, col = "black", cex = 1)
      }
    }

    if (mode == "sqrt")
      A  <- sqrt(energy[n] * f)

    if (mode == "log")
      A  <- log(energy[n] * f)

    if (mode == "linear")
      A  <- energy[n] * f

    # from Heisenberg rule: delta_t x delta_omega >= 1/2
    radius.x = (scale[n] / 2) / shorteningFactor.x
    radius.y = 1 / ((scale[n])) / shorteningFactor.y

    x0 <- position[n]
    y0 <- frequency[n]
    sx <- radius.x
    sy <- radius.y

    # 2D Gaussian
    z <- with(grid,
              A * exp(-((x - x0)^2 / (2 * sx^2) +
                        (y - y0)^2 / (2 * sy^2))))

    # convert to a matrix, because image() requires it
    z.mtx <- matrix(z, nrow = length(t), ncol = length(y))
    tf.map <- tf.map + z.mtx

  } # for (n in 1:num.atoms)

  if(plotAtoms) {
    graphics.off()
    pdf("Atoms.pdf", width = 15, height = 30)

    nn <- num.atoms
    par(mfrow = c(nn, 1), pty = "m")
    # mai: c(bottom, left, top, right)
    par(pty = "m", mai = c(0.05, 4, 0.0, 0.1))
    par(mgp = c(0, 0, 0))
    par(las = 1)
    for (m in 1:num.atoms) {
      if (m %% 2 == 0) cc = "blue" else cc = "red"
      if (m == 1) {
        plot(gabors[, m], xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "l", bty = "n", col = cc)
        lab <- seq(from = 0, to = ceiling(tail(t, 1)), length.out = 11)
        axis(
          side = 1, las = 1, cex.axis = 0.9,
          at = seq(from = 0, to = ceiling(tail(t * f, 1)), length.out = 11),
          labels = c(formatC(lab, format = "f", digits = 2))
        )
      } else {
        plot(gabors[, m], xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "l", bty = "n", col = cc)
      }

      txt <- paste(
        "A", m,
        ", f=",
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
  }

  if (outMode == "plot") {
    if (plotSignals) {
      grid.matrix <- cbind(c(1, 1, 1, 2, 3))
      layout(grid.matrix, widths = c(1, 1, 1), heights = c(2, 1, 1))
      # mai: c(bottom, left, top, right)
      par(pty = "m", mai = c(0.4, 0.7, 0.2, 0.4))
      par(xaxs = "i", yaxs = "i")
    } else {
      par(mfrow = c(1, 1), pty = "m")
      par(mai = c(0.9, 0.9, 0.2, 0.4))
      par(xaxs = "i", yaxs = "i")
    }

    # Drawing with graphics::image() is very slow, especially for large matrices.
    # Graphics::rasterImage() is much faster.

    z.col <- col[cut(tf.map, breaks = 129)]
    # image() and rasterImage() differ in the orientation of the Y axis.
    # Often you also need to do the following:
    rot90 <- function(m) t(m)[ncol(m):1, ]
    tf.map.rot90 <- rot90(matrix(z.col, nrow(tf.map)))
    plot.new()
    plot.window(range(t), range(y))
    rasterImage(tf.map.rot90, 0, 0, tail(t, 1), tail(y, 1))

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
    if (displayAtomNumbers) {
      for (n in 1:num.atoms) {
        text(position[n], frequency[n], n, col = "black", cex = 1)
      }
    }

    # We display small crosses in the centres of atoms
    for (n in 1:num.atoms) {
      if (displayCrosses) {
        points(position[n], frequency[n], pch = 3, col = crossesColor, cex = 1)
      }
    }

    if (displayGrid)
      grid(col = "grey")

    if (plotSignals) {
      xx <- seq(from = 0, to = epochSize / f, length.out =  epochSize)
      plot(x = xx, original.signal, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, main = "Original signal", panel.first = grid())
      abline(h = 0, col = "blue")

      plot(x = xx, reconstruction, type = "l", xlab = "", ylab = "", xaxs = "i", las = 1, main = "Reconstructed signal", panel.first = grid())
      abline(h = 0, col = "blue")
    }

  } # if (outMode == "plot")

  if (outMode == "file") {
    graphics.off()
    png(fileName, width = fileSize[1], height = fileSize[2], pointsize = 18)
    par(pty = "m", mai = c(0, 0, 0, 0))
    graphics::image(x = t, y = y, z = tf.map, col = col)
    dev.off()
  } # if (outMode == "file")

  if (outMode == "RData") {
    im <- as.cimg(tf.map)
    im.resampled <- imager::resize(im, size_x = fileSize[1], size_y = fileSize[2], interpolation_type = 3)
    tf.map.resampled <- as.matrix(im.resampled)

    # Rescaling to the range 0-1.
    # Protect against a situation where a zero appears in the denominator.
    if (max(tf.map.resampled) - min(tf.map.resampled) == 0) {
      tf.map.resampled <- matrix(0, fileSize[1], fileSize[2])
    } else {
      tf.map.resampled <- (tf.map.resampled - min(tf.map.resampled)) / (max(tf.map.resampled) - min(tf.map.resampled))
    }

    # Remove extension
    fileName <- paste(tools::file_path_sans_ext(fileName), ".RData", sep = "")
    save(tf.map.resampled, file = fileName)
  }

  if (outMode == "RData2") {
    zz  <- tf.map
    rr <- raster::raster(nrow = ncol(zz), ncol = nrow(zz)) # # this is how it should be: nrow = ncol(zz), ncol = nrow(zz)
    rr[] <- t(zz)
    tt <- raster::raster(ncol = fileSize[1], nrow = fileSize[2])
    tt <- raster::resample(rr, tt)
    tf.map.resampled <- matrix(tt@data@values, fileSize[1], fileSize[2])

    # Rescaling to the range 0-1.
    # Protect against a situation where a zero appears in the denominator.
    if (max(tf.map.resampled) - min(tf.map.resampled) == 0) {
      tf.map.resampled <- matrix(0, fileSize[1], fileSize[2])
    } else {
      tf.map.resampled <- (tf.map.resampled - min(tf.map.resampled)) / (max(tf.map.resampled) - min(tf.map.resampled))
    }

    # Remove extension
    fileName <- paste(tools::file_path_sans_ext(fileName), ".RData", sep = "")
    save(tf.map.resampled, file = fileName)
  } # if (outMode == "RData")

  # Signal energy
  o <- round(sum(original.signal^2), 2)
  r <- round(sum(reconstruction^2), 2)

  cat("\nEnergy of the original signal:      ",o , "\n", sep = "")
  cat("Signal energy after reconstruction: ",r , "\n", sep = "")
  cat("reconstruction / original %:        ", r / o * 100, "\n", sep = "")

  list(
    gabor.functions = gabors,
    reconstruction = reconstruction,
    original.signal = original.signal,
    f = f,
    epochSize = epochSize,
    number.of.secs = s,
    tf.map = tf.map,
    channel = channel,
    freq.divide = freqDivide)
}
