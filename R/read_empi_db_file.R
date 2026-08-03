#' Reads data from a SQLite file created by the Matching Pursuit algorithm
#'
#' Reads data from a SQLite file (\code{.db}) created by the Matching Pursuit algorithm.
#' The reconstructed signal(s) and Gabor function(s) are also returned.
#'
#' @param db_file A character string giving the path to a SQLite database file.
#'
#' @return  An object of class \code{"mp"} containing:
#' \describe{
#'   \item{atoms}{A data frame describing the selected atoms.}
#'   \item{signal}{Matrix containing the original signal(s).}
#'   \item{reconstruction}{Matrix containing the reconstructed signal(s).}
#'   \item{selected_atoms}{List of matrices containing selected atoms for each channel.}
#'   \item{time}{Time vector corresponding to signal samples.}
#'   \item{sampling_frequency}{Sampling frequency.}
#' }
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbGetQuery
#'
#' @export
#'
#' @examples
#' file <- system.file("extdata", "EEG_filter_resample_montage.db", package = "MatchingPursuit")
#' out <- read_empi_db_file(file)
#'
#' n_channels <- ncol(out$signal)
#' signal <- out$signal
#' reconstruction <- out$reconstruction
#' t <- out$t
#' sampling_frequency <- out$sampling_frequency
#'
#' old.par <- par("mfrow", "pty", "mai")
#'
#' par(mfrow = c(2, 1))
#' par(pty = "m")
#' par(mai = c(0.9, 0.5, 0.3, 0.4))
#'
#' plot(
#'   signal[,1], type = "l", col = "blue",
#'   main = paste("channel: ", 1, " / " , n_channels, " (original signal)",  sep = ""),
#'   xaxt = "n", ylab = "", xlab = "time [sec]"
#' )
#'
#' len <- length(signal[, 1])
#' lab <- seq(t[1], t[len] + 1 / sampling_frequency, length.out = 11)
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
read_empi_db_file <- function(db_file) {

  if (!file.exists(db_file)) {
    stop("Database file does not exist: ", db_file)
  }

  con <- dbConnect(drv = RSQLite::SQLite(), dbname = db_file)

  ## list all tables
  tables <- dbListTables(con)

  ## create a data.frame for each table
  data_frames <- vector("list", length = length(tables))
  names(data_frames) <- tables

  required_tables <- c("metadata", "segments", "samples", "atoms")
  missing_tables <- setdiff(required_tables, names(data_frames))

  if(length(missing_tables) > 0) {
    stop("Missing tables in SQLite file: ", paste(missing_tables, collapse = ", "))
  }

#  for (i in seq(along = tables)) {
#    data_frames[[i]] <- dbGetQuery(conn = con, statement = paste("SELECT * FROM '", tables[[i]], "'", sep = ""))
#  }
  for (table_name in tables) {
    data_frames[[table_name]] <- dbGetQuery(
      conn = con,
      statement = paste0("SELECT * FROM '", table_name, "'", sep = "")
    )
  }

  dbDisconnect(con)

  # sampling rate in Hz
  sampling_frequency <- as.numeric(data_frames[["metadata"]]$value[3])

  # number of samples
  epoch_size <- data_frames[["segments"]]$sample_count

  # number of seconds
  s <- epoch_size / sampling_frequency

  # number of channels
  n_channels <- length(data_frames[["samples"]]$channel_id)

  # parameters of individual atoms
  atoms <- matrix(nrow = length(data_frames[["atoms"]][["segment_id"]]), ncol = 8)
  atoms <- as.data.frame(atoms)
  k <- 0
  for (i in 1:n_channels) {
    # number of atoms. may be different in each channel
    # in empi channels are numbered from 0
    n_atoms <- length(which(data_frames[["atoms"]]$channel_id == (i - 1)))
    for (j in 1:n_atoms) {
      k <- k + 1
      atoms[k, 1] <- i
      atoms[k, 2] <- j
      atoms[k, 3] <- data_frames[["atoms"]][["energy"]][k]
      atoms[k, 4] <- data_frames[["atoms"]][["envelope"]][k]
      atoms[k, 5] <- data_frames[["atoms"]][["f_Hz"]][k]
      atoms[k, 6] <- data_frames[["atoms"]][["phase"]][k]
      atoms[k, 7] <- data_frames[["atoms"]][["scale_s"]][k]
      atoms[k, 8] <- data_frames[["atoms"]][["t0_s"]][k]
    }
  }
  colnames(atoms) <- c("channel_id", "atom_number", "energy", "envelope", "frequency", "phase", "scale", "position")

  # We read the input data from the .db file (they are stored there as float32 numbers)
  # For example: c0 74 23 f3  =  -3.81469

  signal <- matrix(nrow = epoch_size, ncol = n_channels)

  for (k in 1:n_channels) {
    temp <- data_frames[["samples"]][["samples_float32"]][k]
    utemp <- (unlist(temp))
    for (i in 1:epoch_size) {
      b <- readBin(utemp[((i - 1) * 4 + 1) : ((i - 1) * 4 + 4)], "raw", 4)
      # Convert little-endian byte order used by EMPI
      # to the order expected by readBin().
      b2 <- paste(b[4], b[3], b[2], b[1], sep = "")
      # https://stackoverflow.com/questions/39461349/converting-hex-format-to-float-numbers-in-r
      signal[i, k] <-
        readBin(as.raw(strtoi(substring(b2, (step <- seq(1, nchar(b2), by = 2)), step + 1), 16)), "double", n = 1, size = 4)
    }

  }
  # head(atoms)
  # tail(atoms)

  reconstruction <- matrix(0, nrow = epoch_size, ncol = n_channels)
  selected_atoms <- list()

  for (k in 1:n_channels) {
    rows <- which(atoms$channel_id == k)
    atoms_channel <- atoms[rows,]
    colnames(atoms_channel) <- c("channel_id", "atom_number", "energy", "envelope", "frequency", "phase", "scale", "position")
    n_atoms <- length(which(data_frames[["atoms"]]$channel_id == (k - 1)))
    g <- matrix(0, nrow = epoch_size, ncol = n_atoms)
    for (i in 1:n_atoms) {
      gab <- gabor_fun(
        number_of_samples = epoch_size,
        sampling_frequency = sampling_frequency,
        mean = atoms_channel$position[i],
        phase = atoms_channel$phase[i],
        sigma = atoms_channel$scale[i],
        frequency = atoms_channel$frequency[i],
        normalization = TRUE
      )
      reconstruction[, k] <- reconstruction[, k] + gab$gabor * sqrt(atoms_channel$energy[i] * sampling_frequency)

      g[, i] <- gab$gabor
    }
    selected_atoms[[k]] <- g
  }
  output <- list(
    atoms = atoms,
    signal = signal,
    reconstruction = reconstruction,
    selected_atoms = selected_atoms,
    time = gab$t,
    sampling_frequency = sampling_frequency)

  class(output) <- "mp"
  return(output)

}

