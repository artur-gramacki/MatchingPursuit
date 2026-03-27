#' Reads data from a SQLite file created by the Matching Pursuit algorithm
#'
#' Reads data from a SQLite file (\code{.db}) created by the Matching Pursuit algorithm.
#' The reconstructed signal(s) and Gabor function(s) are also returned. These features
#' are not stored directly in the SQLite file; instead, they are computed based on
#' the parameters of individual atoms.
#'
#'@param db.file SQLite file.
#'
#' @return
#' \itemize{
#'    \item Detailed parameters of all the generated atoms.
#'    \item Original input signal(s).
#'    \item Reconstructed signal(s), as the sum of generated atoms.
#'    \item Generated Gabor atoms.
#' }
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbGetQuery
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "EEG.db", package = "MatchingPursuit")
#' out <- read.empi.db.file(file)
#'
#' n.channnels <- ncol(out$original.signal)
#' original.signal <- out$original.signal
#' reconstruction <- out$reconstruction
#' t <- out$t
#' f <- out$f
#'
#' old.par <- par("mfrow", "pty", "mai")
#'
#' par(mfrow = c(2, 1))
#' par(pty = "m")
#' par(mai = c(0.9, 0.5, 0.3, 0.4))
#'
#' plot(
#'   original.signal[,1], type = "l", col = "blue",
#'   main = paste("channel: ", 1, " / " , n.channnels, " (original signal)",  sep = ""),
#'   xaxt = "n", ylab = "", xlab = "time [sec]"
#' )
#'
#' len <- length(original.signal[, 1])
#' lab <- seq(t[1], t[len] + 1 / f, length.out = 11)
#' axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
#'
#' plot(
#'   reconstruction[,1], type = "l", col = "blue",
#'   main = paste("channel: ", 1, " / " , n.channnels, " (reconstructed signal)",  sep = ""),
#'   xaxt = "n", ylab = "", xlab = "time [sec]"
#' )
#'
#' axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
#'
#' par(old.par)
#'
#' ## End(Not run)
#'
read.empi.db.file <- function(db.file) {

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = db.file)

  ## list all tables
  tables <- dbListTables(con)

  ## create a data.frame for each table
  data.frames <- vector("list", length = length(tables))

   for (i in seq(along = tables)) {
    data.frames[[i]] <- dbGetQuery(conn = con, statement = paste("SELECT * FROM '", tables[[i]], "'", sep = ""))
  }

  dbDisconnect(con)

  # sampling rate in Hz
  f <- as.numeric(data.frames[[2]]$value[3])

  # number of samples
  epochSize <- data.frames[[4]]$sample_count

  # number of seconds
  s <- epochSize / f

  # number of channels
  n.channnels <- length(data.frames[[3]]$channel_id)

  # parameters of individual atoms
  atoms <- matrix(nrow = length(data.frames[[1]][["segment_id"]]), ncol = 9)
  atoms <- as.data.frame(atoms)
  k <- 0
  for (i in 1:n.channnels) {
    # number of atoms. may be different in each channel
    # in empi channels are numbered from 0
    n.atoms <- length(which(data.frames[[1]]$channel_id == (i - 1)))
    for (j in 1:n.atoms) {
      k <- k + 1
      atoms[k, 1] <- i
      atoms[k, 2] <- j
      atoms[k, 3] <- data.frames[[1]][["amplitude"]][k]
      atoms[k, 4] <- data.frames[[1]][["energy"]][k]
      atoms[k, 5] <- data.frames[[1]][["envelope"]][k]
      atoms[k, 6] <- data.frames[[1]][["f_Hz"]][k]
      atoms[k, 7] <- data.frames[[1]][["phase"]][k]
      atoms[k, 8] <- data.frames[[1]][["scale_s"]][k]
      atoms[k, 9] <- data.frames[[1]][["t0_s"]][k]
    }
  }
  colnames(atoms) <- c("channel_id", "atom_number", "amplitude", "energy", "envelope", "frequency", "phase", "scale", "position")

  # We read the input data from the .db file (they are stored there as float32 numbers)
  # For example: c0 74 23 f3  =  -3.81469

  original.signal <- matrix(nrow = epochSize, ncol = n.channnels)

  for (k in 1:n.channnels) {
    temp <- data.frames[[3]][["samples_float32"]][k]
    utemp <- (unlist(temp))
    for (i in 1:epochSize) {
      (b <- readBin(utemp[((i - 1) * 4 + 1) : ((i - 1) * 4 + 4)], "raw", 4))
      # swap to use big-endian
      (b2 <- paste(b[4], b[3], b[2], b[1], sep = ""))
      # https://stackoverflow.com/questions/39461349/converting-hex-format-to-float-numbers-in-r
      original.signal[i, k] <-
        readBin(as.raw(strtoi(substring(b2, (step <- seq(1, nchar(b2), by = 2)), step + 1), 16)), "double", n = 1, size = 4)
    }

  }
  # head(atoms)
  # tail(atoms)

  reconstruction <- matrix(0, nrow = epochSize, ncol = n.channnels)
  gabors <- list()

  for (k in 1:n.channnels) {
    rows <- which(atoms$channel_id == k)
    atoms.channel <- atoms[rows,]
    colnames(atoms.channel) <- c("channel_id", "atom_number", "amplitude", "energy", "envelope", "frequency", "phase", "scale", "position")
    n.atoms <- length(which(data.frames[[1]]$channel_id == (k - 1)))
    g <- matrix(0, nrow = epochSize, ncol = n.atoms)
    for (i in 1:n.atoms) {
      gab <- gabor.fun(
        number.of.samples = epochSize,
        sampling.frequency = f,
        mean = atoms.channel$position[i],
        phase = atoms.channel$phase[i],
        sigma = atoms.channel$scale[i],
        frequency = atoms.channel$frequency[i],
        normalization = T
      )
      reconstruction[, k] <- reconstruction[, k] + gab$gabor * sqrt(atoms.channel$energy[i] * f)

      g[, i] <- gab$gabor
    }
    gabors[[k]] <- g
  }
  list(
    atoms = atoms,
    original.signal = original.signal,
    reconstruction = reconstruction,
    gabors = gabors,
    t = gab$t,
    f = f)
}

