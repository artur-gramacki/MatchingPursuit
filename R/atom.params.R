#' Reading the atom parameters
#'
#' Returns a data frame with atom parameters read from a SQLite file.
#'
#' @param db.file The SQLite file created after executing the \code{empi.execute()} function.
#'
#' @return Data frame with all the atom parameters saved in a given SQLite file.
#' The file can be generated using the \code{empi.execute()} function.
#'
#' @export
#'
#' @examples
#' # The file contains data with 18 channels.
#' file <- system.file("extdata", "EEG.db", package = "MatchingPursuit")
#' out <- atom.params(file)
#' out[which(out$channel_id == 1), ]
#' out[which(out$channel_id == 18), ]
#'
#' # This file contains data with only 1 channel.
#' file <- system.file("extdata", "sample1.db", package = "MatchingPursuit")
#' out <- atom.params(file)
#' out
#'
atom.params <- function(db.file) {
  out <- read.empi.db.file(db.file)

  atoms <- data.frame(
    channel_id = out$atoms$channel_id,
    atom_number = out$atoms$atom_number,
    amplitude = out$atoms$amplitude,
    energy = out$atoms$energy,
    frequency = out$atoms$frequency,
    phase = out$atoms$phase,
    scale = out$atoms$scale,
    position = out$atoms$position
  )

  atoms <- round(as.data.frame(atoms), 3)
  colnames(atoms) <- c("channel_id", "atom_number", "amplitude", "energy", "frequency", "phase", "scale", "position")
  return(atoms)
}

