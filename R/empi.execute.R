#' Launches the empi program
#'
#' Runs the EMPI program for the given data (signal).
#'
#' @details
#' The EMPI program (source code and binary files for multiple operating systems) can be
#' downloaded from \url{https://github.com/develancer/empi}. Details are presented in the
#' journal paper: Różański, P. T. (2024). \emph{empi: GPU-Accelerated Matching Pursuit with
#' Continuous Dictionaries}. ACM Transactions on Mathematical Software, Volume 50, Issue 3,
#' Article No. 17, pp. 1-17, \doi{10.1145/3674832}.
#'
#' @param signal List containing the signal in a data frame together with its sampling frequency.
#' The data frame should have meaningful column names (channel names).
#' The list must contain elements named \code{"signal"} and \code{"sampling.rate"}.
#'
#'
#' @param empi.options If \code{NULL}, the EMPI program is run with
#' \code{"-o local --gabor -i 50"} parameters. Otherwise, the user may specify any command-line
#' options. See the \code{README.md} file after downloading the EMPI program using the
#' \code{empi.install()} function.
#'
#' @param  write.to.file If \code{TRUE}, a SQLite database file will be created
#' and saved in the \code{path} directory or, if \code{path = NULL}, in the
#' cache directory. This file stores the results of signal decomposition using the MP algorithm
#'
#' @param path Directory in which the SQLite database file will be saved.
#' If \code{NULL}, the file will be saved in the cache directory.
#'
#' @param file.name Name of the file to create if \code{write.to.file = TRUE}.
#'
#' @return Results of signal decomposition using the MP algorithm. An object of class
#' \code{empi} is returned. If \code{write.to.file = TRUE}, the results are also written
#' to a SQLite file in the \code{path} directory.
#'
#' @export
#'
#' @examples
#' ## Not run:
#' file <- system.file("extdata", "sample1.csv", package = "MatchingPursuit")
#' out <- read.csv.signals(file)
#'
#' out.empi <- empi.execute(
#'   signal = out,
#'   empi.options = NULL,
#'   write.to.file = FALSE,
#'   path = NULL,
#'   file.name = NULL
#' )
#'
#' plot(out.empi)
#' ## End(Not run)
#'
empi.execute <- function(
    signal,
    empi.options = NULL,
    write.to.file = FALSE,
    path = NULL,
    file.name = NULL)
{

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
