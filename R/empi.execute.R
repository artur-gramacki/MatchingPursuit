#' Launches the empi program
#'
#' Runs the \emph{empi} program for the given data (signal).
#'
#' @details
#' The \emph{empi} program (source and binary files for various operating systems) can be
#' downloaded from \url{https://github.com/develancer/empi}. Details are presented in the
#' journal paper: Różański, P.T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with
#' Continuous Dictionaries}.ACM Transactions on Mathematical Software, Volume 50, Issue 3,
#' Article No. 17, pp. 1-17, \doi{10.1145/3674832}.
#'
#' @param signal List returned from \code{read.csv.signals()} function. The list stores
#' signal(s) in a data frame: rows = samples for all channels, columns = channels and
#' sampling frequency. The data should have logical column names (channel names).
#'
#'
#' @param empi.options If \code{NULL}, the \emph{empi} program runs with
#' \code{"-o local --gabor -i 50"} parameters. Otherwise, user can specify any command-line
#' options. See \code{README.md} file after downloading the \emph{empi} program using
#' \code{empi.download()} function.
#'
#' @param  write.to.file If \code{TRUE}, a SQLite database file will be created
#' and saved in the \code{file.dest} directory or, (if \code{file.dest=NULL}), in the
#' cache directory. This file stores the results of signal decomposition using the MP algorithm.
#'
#' @param file.dest Directory where to save a SQLite database file.
#' If \code{file.dest=NULL}), the file is saved in the cache directory.
#'
#' @param file.name The name of the file to generate if \code{write.to.file=TRUE}.
#'
#' @return Signal decomposition results using the MP algorithm. If \code{write.to.file=TRUE}
#' is specified, the results are additionally written to the file \code{.db} on disk
#' in the working directory.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' signal <- read.csv.signals(file)
#'
#' empi.out <- empi.execute (
#'   signal = signal,
#'   empi.options = NULL,
#'   write.to.file = FALSE,
#'   file.dest = NULL,
#'   file.name = NULL
#' )
#' ## End(Not run)
#'
empi.execute <- function(signal, empi.options = NULL, write.to.file = FALSE, file.dest = NULL, file.name = NULL) {

  empi.path <- empi.check()

  sig <- signal[[1]]
  sampling.rate <- signal[[2]]

  n.channels <- ncol(sig)

  signal.raw <- sig2bin(data = sig, write.to.file = FALSE)

  file.bin <- tempfile(fileext = ".bin")
  file.db <- tempfile(fileext = ".db")

  writeBin(signal.raw, file.bin)

  if (is.null(empi.options)) {
    options <-  "-o local --gabor -i 50"
  } else {
    options <- empi.options
  }

  command <- paste(
    empi.path,
    " ",
    file.bin,
    " ",
    file.db,
    " ",
    "-f ",
    sampling.rate,
    " -c ",
    n.channels,
    " --channels 1-",
    n.channels,
    " ",
    options,
    sep = "")

  system(command)

  if (write.to.file) {

    if (is.null(file.dest)) {
      dest.dir <- file.path(tools::R_user_dir("MatchingPursuit", "cache"))
      dest.dir <- normalizePath(dest.dir, winslash = "/")
    } else {
      dest.dir <- file.dest
    }

    if (is.null(file.name)) {
      temp <- paste(dest.dir, "/empi.db", sep = "")
      file.copy(file.db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition were saved in the '", temp, "' file (in SQLite format).")
    } else {
      temp <- paste(dest.dir, "/", file.name, sep = "")
      file.copy(file.db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition were saved in the '", temp, "' file (in SQLite format).", sep = "")
    }
  }

  out <- read.empi.db.file(file.db)
  file.remove(file.bin, file.db)

  return(out)
}
