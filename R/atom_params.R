#' Read atom parameters from a SQLite database
#'
#' Reads atom parameters stored in a SQLite database created by \code{empi_execute()} function.
#'
#' @param db_file A character string giving the path to a SQLite database file.
#'
#' @return A data frame containing the atom parameters stored in the database.
#'
#' @export
#'
#' @examples
#' # Example database containing data from 18 channels
#' file <- system.file("extdata", "EEG.db", package = "MatchingPursuit")
#' out <- atom_params(file)
#' out[which(out$channel_id == 1), ]
#' out[which(out$channel_id == 18), ]
#'
#' # Example database containing data from a single channel
#' file <- system.file("extdata", "sample1.db", package = "MatchingPursuit")
#' out <- atom_params(file)
#' out
#'
atom_params <- function(db_file) {

  out <- read_empi_db_file(db_file)

  atoms <- data.frame(
    channel_id = out$atoms$channel_id,
    atom_number = out$atoms$atom_number,
    energy = out$atoms$energy,
    frequency = out$atoms$frequency,
    phase = out$atoms$phase,
    scale = out$atoms$scale,
    position = out$atoms$position
  )

  atoms <- round(as.data.frame(atoms), 3)

  colnames(atoms) <- c(
    "channel_id",
    "atom_number",
    "energy",
    "frequency",
    "phase",
    "scale",
    "position"
  )

  return(atoms)
}
