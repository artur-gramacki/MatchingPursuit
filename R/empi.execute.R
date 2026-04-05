#' Launches the empi program
#'
#' Runs the EMPI program for the given data (signal).
#'
#' @details
#' The EMPI program (source and binary files for various operating systems) can be
#' downloaded from \url{https://github.com/develancer/empi}. Details are presented in the
#' journal paper: Różański, P. T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with
#' Continuous Dictionaries}.ACM Transactions on Mathematical Software, Volume 50, Issue 3,
#' Article No. 17, pp. 1-17, \doi{10.1145/3674832}.
#'
#' @param signal List returned from \code{read.csv.signals()} function. The list stores
#' the signal in a data frame along with its sampling frequency.
#' The data should have logical column names (channel names).
#' The elements of the list must be named \code{"signal"} and \code{"sampling.rate"}.
#'
#'
#' @param empi.options If \code{NULL}, the EMPI program runs with
#' \code{"-o local --gabor -i 50"} parameters. Otherwise, user can specify any command-line
#' options. See \code{README.md} file after downloading the EMPI program using
#' \code{empi.install()} function.
#'
#' @param  write.to.file If \code{TRUE}, a SQLite database file will be created
#' and saved in the \code{path} directory or, (if \code{path=NULL}), in the
#' cache directory. This file stores the results of signal decomposition using the MP algorithm.
#'
#' @param path Directory in which to save the SQLite database file.
#' If it is \code{NULL}, the file will be saved in the cache directory.
#'
#' @param file.name The name of the file to generate if \code{write.to.file=TRUE}.
#'
#' @return Results of signal decomposition using the MP algorithm. If \code{write.to.file=TRUE}
#' is specified, the results are also written to a SQLite file on disk in the \code{path}
#' directory.
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
#'   path = NULL,
#'   file.name = NULL
#' )
#' ## End(Not run)
#'
empi.execute <- function(signal, empi.options = NULL, write.to.file = FALSE, path = NULL, file.name = NULL) {

  empi.path <- empi.check()

  if(is.null(empi.path)) {
    return()
  }

  if (!all(c("signal", "sampling.rate") %in% names(signal))) {
    stop("Input list must contain 'signal' and 'sampling.rate'.")
  }

  sig <- signal$signal
  sampling.rate <- signal$sampling.rate

  n.channels <- ncol(sig)

  signal.raw <- sig2bin(data = sig, write.to.file = FALSE)

  file.bin <- tempfile(fileext = ".bin")
  file.db <- tempfile(fileext = ".db")

  # cleanup if error
  on.exit(file.remove(file.bin, file.db), add = TRUE)

  writeBin(signal.raw, file.bin)

  if (is.null(empi.options)) {
    options <-  "-o local --gabor -i 50"
  } else {
    options <- empi.options
  }

  command <- paste(
    shQuote(empi.path),
    " ",
    shQuote(file.bin),
    " ",
    shQuote(file.db),
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

  status <- system(command)

  if (status != 0) {
    stop("EMPI execution failed.", call. = FALSE)
  }

  if (write.to.file) {

    if (is.null(path)) {
      dest.dir <- tools::R_user_dir("MatchingPursuit", "cache")
      dir.create(dest.dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      dest.dir <- path
      if (!dir.exists(dest.dir)) {
        ok <- dir.create(dest.dir, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(path)) {
          stop("Cannot create directory '", dest.dir, "'.")
        }
      }
    }

    if (is.null(file.name)) {
      temp <- file.path(dest.dir, "empi.db")
      file.copy(file.db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition saved to '", temp, "'.")
    } else {
      temp <- file.path(dest.dir, file.name)
      file.copy(file.db, temp, overwrite = TRUE)
      message("Results of the Matching Pursuit decomposition saved to '", temp, "'.")
    }
  }

  out <- read.empi.db.file(file.db)

  return(out)
}
