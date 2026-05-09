#' Read atom parameters from a SQLite database
#'
#' Reads atom parameters stored in a SQLite database created by \code{empi.execute()} function.
#'
#' @param db.file A character string giving the path to a SQLite database file.
#'
#' @return A data frame containing the atom parameters stored in the database.
#'
#' @export
#'
#' @examples
#' # Example database containing data from 18 channels
#' file <- system.file("extdata", "EEG.db", package = "MatchingPursuit")
#' out <- atom.params(file)
#' out[which(out$channel_id == 1), ]
#' out[which(out$channel_id == 18), ]
#'
#' # Example database containing data from a single channel
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

