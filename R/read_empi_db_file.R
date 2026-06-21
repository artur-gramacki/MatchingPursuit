#' Reads data from a SQLite file created by the Matching Pursuit algorithm
#'
#' Reads data from a SQLite file (\code{.db}) created by the Matching Pursuit algorithm.
#' The reconstructed signal(s) and Gabor function(s) are also returned.
#'
#'@param db_file SQLite file.
#'
#' @return  An object of class \code{"mp"} containing:
#'
#' \item{atoms}{A data frame describing the selected atoms.}
#' \item{original_signal}{Matrix containing the original signal(s).}
#' \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#' \item{gabors}{List of matrices containing selected atoms for each channel.}
#' \item{t}{Time vector corresponding to signal samples.}
#' \item{sf}{Sampling frequency.}
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbGetQuery
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "EEG_bipolar_filtered.db", package = "MatchingPursuit")
#' out <- read_empi_db_file(file)
#'
#' n_channels <- ncol(out$original_signal)
#' original_signal <- out$original_signal
#' reconstruction <- out$reconstruction
#' t <- out$t
#' sf <- out$sf
#'
#' old.par <- par("mfrow", "pty", "mai")
#'
#' par(mfrow = c(2, 1))
#' par(pty = "m")
#' par(mai = c(0.9, 0.5, 0.3, 0.4))
#'
#' plot(
#'   original_signal[,1], type = "l", col = "blue",
#'   main = paste("channel: ", 1, " / " , n_channels, " (original signal)",  sep = ""),
#'   xaxt = "n", ylab = "", xlab = "time [sec]"
#' )
#'
#' len <- length(original_signal[, 1])
#' lab <- seq(t[1], t[len] + 1 / sf, length.out = 11)
#' axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
#'
#' plot(
#'   reconstruction[,1], type = "l", col = "blue",
#'   main = paste("channel: ", 1, " / " , n_channels, " (reconstructed signal)",  sep = ""),
#'   xaxt = "n", ylab = "", xlab = "time [sec]"
#' )
#'
#' axis(side = 1, las = 1, cex.axis = 0.9, at = seq(0, len, length.out = 11), labels = lab)
#'
#' par(old.par)
#'
#' ## End(Not run)
#'
read_empi_db_file <- function(db_file) {

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = db_file)

  ## list all tables
  tables <- dbListTables(con)

  ## create a data.frame for each table
  data_frames <- vector("list", length = length(tables))

  for (i in seq(along = tables)) {
    data_frames[[i]] <- dbGetQuery(conn = con, statement = paste("SELECT * FROM '", tables[[i]], "'", sep = ""))
  }

  dbDisconnect(con)

  # sampling rate in Hz
  sf <- as.numeric(data_frames[[2]]$value[3])

  # number of samples
  epoch_size <- data_frames[[4]]$sample_count

  # number of seconds
  s <- epoch_size / sf

  # number of channels
  n_channels <- length(data_frames[[3]]$channel_id)

  # parameters of individual atoms
  atoms <- matrix(nrow = length(data_frames[[1]][["segment_id"]]), ncol = 8)
  atoms <- as.data.frame(atoms)
  k <- 0
  for (i in 1:n_channels) {
    # number of atoms. may be different in each channel
    # in empi channels are numbered from 0
    n_atoms <- length(which(data_frames[[1]]$channel_id == (i - 1)))
    for (j in 1:n_atoms) {
      k <- k + 1
      atoms[k, 1] <- i
      atoms[k, 2] <- j
      atoms[k, 3] <- data_frames[[1]][["energy"]][k]
      atoms[k, 4] <- data_frames[[1]][["envelope"]][k]
      atoms[k, 5] <- data_frames[[1]][["f_Hz"]][k]
      atoms[k, 6] <- data_frames[[1]][["phase"]][k]
      atoms[k, 7] <- data_frames[[1]][["scale_s"]][k]
      atoms[k, 8] <- data_frames[[1]][["t0_s"]][k]
    }
  }
  colnames(atoms) <- c("channel_id", "atom_number", "energy", "envelope", "frequency", "phase", "scale", "position")

  # We read the input data from the .db file (they are stored there as float32 numbers)
  # For example: c0 74 23 f3  =  -3.81469

  original_signal <- matrix(nrow = epoch_size, ncol = n_channels)

  for (k in 1:n_channels) {
    temp <- data_frames[[3]][["samples_float32"]][k]
    utemp <- (unlist(temp))
    for (i in 1:epoch_size) {
      (b <- readBin(utemp[((i - 1) * 4 + 1) : ((i - 1) * 4 + 4)], "raw", 4))
      # swap to use big-endian
      (b2 <- paste(b[4], b[3], b[2], b[1], sep = ""))
      # https://stackoverflow.com/questions/39461349/converting-hex-format-to-float-numbers-in-r
      original_signal[i, k] <-
        readBin(as.raw(strtoi(substring(b2, (step <- seq(1, nchar(b2), by = 2)), step + 1), 16)), "double", n = 1, size = 4)
    }

  }
  # head(atoms)
  # tail(atoms)

  reconstruction <- matrix(0, nrow = epoch_size, ncol = n_channels)
  gabors <- list()

  for (k in 1:n_channels) {
    rows <- which(atoms$channel_id == k)
    atoms_channel <- atoms[rows,]
    colnames(atoms_channel) <- c("channel_id", "atom_number", "energy", "envelope", "frequency", "phase", "scale", "position")
    n_atoms <- length(which(data_frames[[1]]$channel_id == (k - 1)))
    g <- matrix(0, nrow = epoch_size, ncol = n_atoms)
    for (i in 1:n_atoms) {
      gab <- gabor_fun(
        number_of_samples = epoch_size,
        sampling_frequency = sf,
        mean = atoms_channel$position[i],
        phase = atoms_channel$phase[i],
        sigma = atoms_channel$scale[i],
        frequency = atoms_channel$frequency[i],
        normalization = T
      )
      reconstruction[, k] <- reconstruction[, k] + gab$gabor * sqrt(atoms_channel$energy[i] * sf)

      g[, i] <- gab$gabor
    }
    gabors[[k]] <- g
  }
  output <- list(
    atoms = atoms,
    original_signal = original_signal,
    reconstruction = reconstruction,
    gabors = gabors,
    t = gab$t,
    sf = sf)

  class(output) <- 'mp'
  return(output)

}

